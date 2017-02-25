#include <stdlib.h>
#include "sox.h"
#include <strings.h>

static SockCB *_sockInit(const char *host, const char *service, int &rc);
static int     _wait(int port, unsigned int mSec, bool onRead, bool &result);
static int     _readAll(SockCB *sockCB, char *buff, int size);
static void    _setError(SockCB *sockCB);
static void    _forceClose(SockCB *sockCB);

int soxDuplicate(SockCB *&sockCB, int commsPort, int retries, int timeOut)
{
   sockCB = (SockCB *) malloc(sizeof(SockCB));
   if (sockCB)
   {
      sockCB->listenPort = -1;
      sockCB->commsPort  = commsPort;
      sockCB->retries    = (unsigned short)retries;
      sockCB->timeOut    = timeOut;
      return 0;
   }
   return errSockInit;
}

int soxDupServer(SockCB *&sockCB, int listenPort, int retries, const int timeOut)
{
   sockCB = (SockCB *) malloc(sizeof(SockCB));
   if (sockCB)
   {
      sockCB->listenPort = listenPort;
      sockCB->commsPort  = -1;
      sockCB->retries    = (unsigned short)retries;
      sockCB->timeOut    = timeOut;
      return 0;
   }
   return errSockInit;
}

int soxServerInit(SockCB *&sockCB, const char *service)
{
   int rc;
   sockCB = _sockInit(0, service, rc);
   return rc;
}

int soxServerOpen(SockCB *sockCB)
{
   if (sockCB == 0)
      return errSockInit;
   sockCB->AddrLen   = sizeof(sockCB->Addr);
   sockCB->CommsPort = accept(sockCB->ListenPort, (sockaddr*)&sockCB->Addr, &sockCB->AddrLen);
   if (sockCB->CommsPort == -1)
   {
      _setError(SockCB);
      return errSockSocket;
   }
#if defined(TCP_NODELAY)
   int on = 1;
   setsockopt(sockCB->CommsPort, SOL_SOCKET, TCP_NODELAY,(char*)&on, sizeof(on));
#endif
   return 0;
}

int soxClientInit(SockCB *&sockCB, const char *aHost, const char *service)
{
   int rc;
   sockCB = _sockInit(aHost, service, &rc);
   return rc;
}

static void _forceClose(SockCB *sockCB)
{
   if (sockCB->CommsPort != -1)
      close(sockCB->CommsPort);
   sockCB->CommsPort = -1;
}

int soxClientOpen(SockCB *sockCB)
{
   int rc;

   if (SockCB == &ABNSockCB)
      return errSockInit;

   sockCB->CommsPort = socket(AF_INET, SOCK_STREAM, 0);

   if (sockCB->CommsPort == -1)
   {
      _SetError(SockCB);
      return errSockSocket;
   }

   rc = connect(sockCB->CommsPort, (sockaddr*)&sockCB->Addr, sizeof(sockCB->Addr));

   if (rc)
   {
      _SetError(SockCB);
      _ForceClose(SockCB);
      return errSockConnect;
   }

#if defined(TCP_NODELAY)
   int dealy_on = 1;

   setsockopt(sockCB->CommsPort, SOL_SOCKET, TCP_NODELAY,(char*)&dealy_on, sizeof(dealy_on));
#endif

#if defined(SO_USELOOPBACK)
   int loop_on = 1;

   setsockopt(sockCB->CommsPort, SOL_SOCKET, SO_USELOOPBACK, (char*)&loop_on, sizeof(loop_on));
#endif

   return 0;
}

int soxSetTimeOut(SockCB *sockCB, const int aTimeOut)
{
   if (SockCB == &ABNSockCB)
      return errSockInit;

   sockCB->TimeOut = aTimeOut;
   return 0;
}

int soxSetRetries(SockCB *sockCB, const unsigned short aRetries)
{
   if (SockCB == &ABNSockCB)
      return errSockInit;

   sockCB->Retries = aRetries;
   return 0;
}

int soxDone(SockCB *sockCB)
{
   if (SockCB == &ABNSockCB)
      return errSockInit;

   if (sockCB->CommsPort != -1)
      soclose(sockCB->CommsPort);

   if (sockCB->ListenPort != -1)
      soclose(sockCB->ListenPort);

   sockCB->CommsPort  = -1;
   sockCB->ListenPort = -1;
   return 0;
}

int soxClose(SockCB *sockCB)
{
   if (SockCB == &ABNSockCB)
      return errSockInit;

#if defined(SO_LINGER)

   struct linger x = {1, 5};
   setsockopt(sockCB->CommsPort, SOL_SOCKET, SO_LINGER,(char*)&x, sizeof(x));

#endif

   if (sockCB->CommsPort != -1)
   {
#if defined(SD_BOTH)
      shutdown(sockCB->CommsPort, SD_BOTH); // Vince adding this to see if WAITS reduce faster
#elif defined(SHUT_RDWR)
      shutdown(sockCB->CommsPort, SHUT_RDWR); // Vince adding this to see if WAITS reduce faster
#endif
      soclose(sockCB->CommsPort);
   }
   sockCB->CommsPort = -1;

   return 0;
}

int soxReadLength(const SockCB *sockCB, unsigned int *aSize)
{
   unsigned int Size;

   if (SockCB == &ABNSockCB)
      return errSockInit;

   int n = _ReadAll(SockCB, (char *)&Size, sizeof(Size));

   if (n != sizeof(unsigned int))
      return errSockRead;

   *aSize = ntohl(Size);
   return 0;
}

int soxRead(const SockCB       *sockCB,
                     void                *aBuffer,
                     const unsigned int  aBufferLen)
{
   if (SockCB == &ABNSockCB)
      return errSockInit;

   int n = _ReadAll(SockCB, (char *)aBuffer, aBufferLen);

   if ((unsigned int)n != aBufferLen)
      return errSockRead;

   return 0;
}

int soxStreamRead(const SockCB       *sockCB,
                           void                *aBuffer,
                           const unsigned int  aBufferLen,
                           unsigned int       *aSize)
{
   if (SockCB == &ABNSockCB)
      return errSockInit;

   int n = recv(sockCB->CommsPort, (char *) aBuffer, aBufferLen, 0);
   if (n < 0)
      return errSockRead;
   *aSize = n;
   return 0;
}

int soxWaitRead(const SockCB *sockCB, const unsigned int aTimeOut, bool *aResult)
{
   if (SockCB == &ABNSockCB)
      return errSockInit;

   return _Wait(sockCB->CommsPort, aTimeOut, true, aResult);
}

int soxWaitWrite(const SockCB *sockCB, const unsigned int aTimeOut, bool *aResult)
{
   if (SockCB == &ABNSockCB)
      return errSockInit;

   return _Wait(sockCB->CommsPort, aTimeOut, false, aResult);
}

int soxWaitServer(const SockCB *sockCB, const unsigned int aTimeOut, bool *aResult)
{
   if (SockCB == &ABNSockCB)
      return errSockInit;

   return _Wait(sockCB->ListenPort, aTimeOut, true, aResult);
}

int soxWriteLength(SockCB *sockCB, const unsigned int aLength)
{
   unsigned int writeLength = htonl(aLength);

   int n;

   if (SockCB == &ABNSockCB)
      return errSockInit;

   n = send(sockCB->CommsPort, (char *)&writeLength, sizeof(writeLength), 0);

   if (n != sizeof(writeLength))
   {
      _SetError(SockCB);
      return errSockWrite;
   }

   return 0;
}

int soxWrite(SockCB *sockCB, void *aBuffer, const unsigned int aBufferLen)
{
   int n;

   if (SockCB == &ABNSockCB)
      return errSockInit;

   n = send(sockCB->CommsPort, (char *)aBuffer, aBufferLen, 0);

   if ((unsigned int)n != aBufferLen)
   {
      _SetError(SockCB);
      return errSockWrite;
   }

   return 0;
}

int soxError(const SockCB *sockCB)
{
   return sockCB->ErrorNo;
}

char *soxErrorMsg(const SockCB *sockCB)
{
   return SockErrorText(sockCB->ErrorNo);
}

int soxErrorMsgNet(const SockCB *sockCB, char *Buffer, int BufferLen)
{
   int i;

   strncpy(Buffer, SockErrorMsg(SockCB), BufferLen-1);

   Buffer[BufferLen-1] = 0;

   for (i = strlen(Buffer); i < BufferLen; i++)
      Buffer[i] = ' ';

   return sockCB->ErrorNo;
}

static SockCB *_sockInit(const char *aHost, const char *service, int *rc)
{
   SockCB         *SockCB = &ABNSockCB;
   servent         *ps;
   hostent         *ph;
   sockaddr_in      Addr;

   int ListenPort = -1;
   int isNo, i;

#if defined(M_DOS) || defined(M_W32)

   WSADATA  wsadata;

   *rc = WSAStartup(DESIRED_WINSOCK_VERSION, &wsadata);

   if (*rc)
   {
      *rc = sockCB->ErrorNo = WSAGetLastError();
      return SockCB;
   }
   /* Check that the windows socket dll version is ok */
   if (wsadata.wVersion < MINIMUM_WINSOCK_VERSION)
   {
      *rc = errSockVersion;
      _SetError(SockCB);
      return SockCB;
   }
#endif
   *rc = 0;

   for (isNo = 1, i = 0; service[i]; i++)
   {
      if (service[i] < '0' || service[i] > '9')
      {
         isNo = 0;
         break;
      }
   }

   if (isNo == 1)
      Addr.sin_port = htons((short)atoi(service));
   else
   {
      ps = getservbyname(service, PROTOCOL);
      if (ps == 0)
      {
         *rc = errSockService;
         _SetError(SockCB);
         return SockCB;
      }
      Addr.sin_port = ps->s_port;
   }

   if (aHost != 0)
   {
      ph = gethostbyname(aHost);
      if (ph == 0)
      {
         *rc = errSockHost;
         _SetError(SockCB);
         return SockCB;
      }
      Addr.sin_family = ph->h_addrtype;
      memcpy(&Addr.sin_addr, ph->h_addr, ph->h_length);
   }
   else
   {
      Addr.sin_addr.s_addr = INADDR_ANY;
      Addr.sin_family = AF_INET;
      ListenPort = socket(AF_INET, SOCK_STREAM, 0);
      if (ListenPort == -1)
      {
         *rc = errSockSocket;
         _SetError(SockCB);
         return SockCB;
      }
      int on=1;
#if defined(SO_REUSEADDR)
      if (setsockopt(ListenPort,SOL_SOCKET,SO_REUSEADDR,(char*)&on,
                     sizeof(on))<0)
      {
         *rc = errSockSocket;
         _SetError(SockCB);
         return SockCB;
      }
#endif
#if defined(NO_SO_REUSEPORT)
      if (setsockopt(ListenPort,SOL_SOCKET,SO_REUSEPORT,(char*)&on,
                     sizeof(on))<0)
      {
         *rc = errSockSocket;
         _SetError(SockCB);
         return SockCB;
      }
#endif
      *rc=bind(ListenPort, (struct sockaddr*)&Addr, sizeof(Addr));
      if (*rc == -1)
      {
         *rc = errSockBind;
         _SetError(SockCB);
         return SockCB;
      }
      *rc=listen(ListenPort, SOCKET_QSIZE);
      if (*rc == -1)
      {
         *rc = errSockListen;
         _SetError(SockCB);
         return SockCB;
      }
      if (Addr.sin_port == 0)
      {
         
#if defined(M_AIX) || defined(M_GNU)
		 socklen_t len = sizeof(Addr);
         getsockname(ListenPort, (struct sockaddr*)&Addr, &len);
#else
         int len = sizeof(Addr);
		 getsockname(ListenPort, (struct sockaddr*)&Addr, (int*)&len);
#endif
      }
   }
   SockCB = (SockCB *) malloc(sizeof(SockCB));

   if (SockCB != 0)
   {
      sockCB->ListenPort = ListenPort;
      sockCB->CommsPort = -1;
      sockCB->Retries = 5;
      sockCB->TimeOut = 10000;
      sockCB->Addr = Addr;
      if (aHost != 0)
         sockCB->Addr.sin_addr.s_addr = *((unsigned int *)ph->h_addr);
      else
         sockCB->Addr.sin_addr.s_addr = INADDR_ANY;
      sockCB->Addr.sin_family = AF_INET;
   }
   return SockCB;
}

static int _Wait(const int            aPort,
                 const unsigned int  aMSec,
                 const bool           onRead,
                 bool                *aResult)
{
   struct timeval TimeOut;
   struct timeval *TOP;
   if (aMSec)
   {
      TimeOut.tv_sec  = aMSec / 1000;
      TimeOut.tv_usec = (aMSec % 1000) * 1000;
      TOP = &TimeOut;
   }
   else
      TOP = 0;
   fd_set OnSet;
   FD_ZERO(&OnSet);
   FD_SET((unsigned)aPort, &OnSet);
   int rc = select(FD_SETSIZE, (onRead == true?&OnSet:0), (onRead==true?0:&OnSet), 0, TOP);
   if (rc <= 0)
      return errSockTimeout;
   *aResult = FD_ISSET(aPort, &OnSet) ? true : false;
   return 0;
}

static int _ReadAll(const SockCB *SockCB, char *Buff, const int Size)
{
   int BytesRead = 0, n, Try = 0;
   bool Result;

   while (BytesRead < Size)
   {
      if (sockCB->TimeOut)
      {
         if (_Wait(sockCB->CommsPort, sockCB->TimeOut, true, &Result)==0)
         {
            if (Result == false)
            {
               if (Try++ < sockCB->Retries)
                  continue;
               else
                  return -1;
            }
         }
         else
         {
            return -1;
         }
      }

      n = recv(sockCB->CommsPort, Buff, Size - BytesRead, 0);

      if (n < 0) // underflow ??
         return n;

      if (n == 0)
         return BytesRead;

      BytesRead += n;
      Buff      += n;
   }

   return BytesRead;
}

static void _SetError(SockCB *sockCB)
{
   sockCB->ErrorNo = errno;
}

char *soxErrorText(const int rc)
{
   switch (rc)
   {
      case EPERM:            return "Operation not permitted";
      case ENOENT:           return "No such file or directory";
      case ESRCH:            return "No such process";
      case EINTR:            return "interrupted system call";
      case EIO:              return "I/O error";
      case ENXIO:            return "No such device or address";
      case E2BIG:            return "Arg list too long";
      case ENOEXEC:          return "Exec format error";
      case EBADF:            return "Bad file descriptor";
      case ECHILD:           return "No child processes";
      case EAGAIN:           return "Resource temporarily unavailable";
      case ENOMEM:           return "Not enough space";
      case EACCES:           return "Permission denied";
      case EFAULT:           return "Bad address";
      case ENOTBLK:          return "Block device required";
      case EBUSY:            return "Resource busy";
      case EEXIST:           return "File exists";
      case EXDEV:            return "Improper link";
      case ENODEV:           return "No such device";
      case ENOTDIR:          return "Not a directory";
      case EISDIR:           return "Is a directory";
      case EINVAL:           return "Invalid argument";
      case ENFILE:           return "Too many open files in system";
      case EMFILE:           return "Too many open files";
      case ENOTTY:           return "Inappropriate I/O control operation";
      case ETXTBSY:          return "Text file busy";
      case EFBIG:            return "File too large";
      case ENOSPC:           return "No space left on device";
      case ESPIPE:           return "Invalid seek";
      case EROFS:            return "Read only file system";
      case EMLINK:           return "Too many links";
      case EPIPE:            return "Broken pipe";
      case EDOM:             return "Domain error within math function";
      case ERANGE:           return "Result too large";
      case ENOMSG:           return "No message of desired type";
      case EIDRM:            return "Identifier removed";
      case ECHRNG:           return "Channel number out of range";
      case EL2NSYNC:         return "Level 2 not synchronized";
      case EL3HLT:           return "Level 3 halted";
      case EL3RST:           return "Level 3 reset";
      case ELNRNG:           return "Link number out of range";
      case EUNATCH:          return "Protocol driver not attached";
      case ENOCSI:           return "No CSI structure available";
      case EL2HLT:           return "Level 2 halted";
      case EDEADLK:          return "Resource deadlock avoided";
      case ENOLCK:           return "No locks available";
      case ESTALE:           return "no filesystem";
      case EINPROGRESS:      return "Operation now in progress";
      case EALREADY:         return "Operation already in progress";
      case ENOTSOCK:         return "Socket operation on non-socket";
      case EDESTADDRREQ:     return "Destination address required";
      case EMSGSIZE:         return "Message too long";
      case EPROTOTYPE:       return "Protocol wrong type for socket";
      case ENOPROTOOPT:      return "Protocol not available";
      case EPROTONOSUPPORT:  return "Protocol not supported";
      case ESOCKTNOSUPPORT:  return "Socket type not supported";
      case EOPNOTSUPP:       return "Operation not supported on socket";
      case EPFNOSUPPORT:     return "Protocol family not supported";
      case EAFNOSUPPORT:     return "Address family not supported by protocol family";
      case EADDRINUSE:       return "Address already in use";
      case EADDRNOTAVAIL:    return "Can't assign requested address";
      case ENETDOWN:         return "Network is down";
      case ENETUNREACH:      return "Network is unreachable";
      case ENETRESET:        return "Network dropped connection on reset";
      case ECONNABORTED:     return "Software caused connection abort";
      case ECONNRESET:       return "Connection reset by peer";
      case ENOBUFS:          return "No buffer space available";
      case EISCONN:          return "Socket is already connected";
      case ENOTCONN:         return "Socket is not connected";
      case ESHUTDOWN:        return "Can't send after socket shutdown";
      case ETIMEDOUT:        return "Connection timed out";
      case ECONNREFUSED:     return "Connection refused";
      case EHOSTDOWN:        return "Host is down";
      case EHOSTUNREACH:     return "No route to host";
      case ERESTART:         return "restart the system call";
      case EUSERS:           return "Too many users";
      case ELOOP:            return "Too many levels of symbolic links";
      case ENAMETOOLONG:     return "File name too long";
      case EREMOTE:          return "Item is not local to host";
      case ENOSYS:           return "Function not implemented  POSIX";
      case ETOOMANYREFS:     return "Too many references: can't splice";
      case EILSEQ:           return "Invalid wide character";
      case ECANCELED:        return "asynchronous i/o cancelled";
      case ENOSR:            return "temp out of streams resources";
      case ETIME:            return "I_STR ioctl timed out";
      case EBADMSG:          return "wrong message type at stream head";
      case EPROTO:           return "STREAMS protocol error";
      case ENODATA:          return "no message ready at stream head";
      case ENOSTR:           return "fd is not a stream";
      case EMULTIHOP:        return "multihop is not allowed";
      case ENOLINK:          return "the link has been severed";
      case EOVERFLOW:        return "value too large to be stored in data type";
   }
   return "Unknown Error";
}
