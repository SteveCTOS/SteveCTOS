#ifndef _SOX_H_
#define _SOX_H_

#include <pwd.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/ioctl.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>

#define soclose close
#define recv(a,b,c,d) read((a), (b), (c))
#define send(a,b,c,d) write((a), (b), (c))

#define PROTOCOL "tcp"
#define SOCKET_QSIZE 5

typedef struct SockCB
{
   int listenPort;           // TCP socket Descriptor for Server
   int commsPort;            // TCP socket Descriptor for active connection
   struct sockaddr_in addr;  // Socket address
   socklen_t addrLen;
   unsigned short retries;   // For Wait
   int timeOut;              // For Wait
   int errorNo;              // Error Code
} SockCB;

enum eError
{
  errSockInit = 1,
  errSockHost,
  errSockService,
  errSockSocket,
  errSockBind,
  errSockListen,
  errSockConnect,
  errSockClose,
  errSockRead,
  errSockWrite,
  errSockVersion,
  errSockIOCtl,
  errSockLinger,
  errSockKeepAlive,
  errSockTimeout,
  errSockDebug,
  errSockGetOpt,
  errSockSetOpt
};

int soxDuplicate(SockCB  *&sockCB, int commsPort, int retries, int timeOut); 
int soxDupServer(SockCB  *&sockCB, int commsPort, int retries, int timeOut); 
int soxServerInit(SockCB *&sockCB, const char *service); 
int soxServerOpen(SockCB  *sockCB);
int soxWaitServer(SockCB  *sockCB, unsigned int timeOut, bool &result);
int soxClientInit(SockCB *&sockCB, const char *host, const char *service);
int soxClientOpen(SockCB  *sockCB);
int soxDone(SockCB       *&sockCB);
int soxClose(SockCB       *sockCB);
int soxReadLength(SockCB  *sockCB, unsigned int &size);
int soxRead(SockCB        *sockCB, void *buffer, unsigned int bufferLen);
int soxStreamRead(SockCB  *sockCB, void *buffer, unsigned int bufferLen, unsigned int &size);
int soxWaitRead(SockCB    *sockCB, unsigned int timeOut, bool &result);
int soxWriteLength(SockCB *sockCB, unsigned int length);
int soxWrite(SockCB       *sockCB, void *buffer, unsigned int bufferLen);
int soxWaitWrite(SockCB   *sockCB, unsigned int timeOut, bool &result);
int soxSetTimeOut(SockC   *sockCB, unsigned int timeOut);
int soxSetRetries(SockCB  *sockCB, unsigned short retries);
int soxError(SockCB       *sockCB);
const char* soxErrorText(int rc);
const char* soxErrorMsg(SockCB *sockCB);
int soxErrorMsgNet(SockCB *sockCB, char *buffer, int bufferLen);

#endif