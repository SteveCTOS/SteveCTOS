#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <ncurses.h>
#include <ctype.h>

#define DEBUG_LOGGING_ON
#ifndef DEBUG_LOGGING_ON
  #define LOG(x)
#else
  static FILE* _logfile_;
  
  static void _log_(char* line)
  {
    if (_logfile_ == 0)
      _logfile_ = fopen("submit.log", "at");
    if (_logfile_ == 0) return;
    fprintf(_logfile_, "%s\n", line);
    fflush(_logfile_);
  }
  
  static char _log_buffer_[4196];
  #define LOG(x) _log_(x);
#endif

const char *_errors_[] =
{ "All Cool"
, "Missing program codes to submit"
, "Program is not executable"
, "Program is not executing"
, "Program closed with RC"
, "Code is not valid hex"
};

enum
{ ERC_OK
, ERC_MISSING_PROGRAM_CODES
, ERC_PROGRAM_NOT_EXECUTABLE
, ERC_PROGRAM_NOT_EXECUTING
, ERC_PROGRAM_CLOSED_WITH_RC
, ERC_CODE_IS_NOT_VALID_HEX
};

static char* _format_(char* work, int len, const char *fmt, ...)
{
  va_list  args;
  va_start(args, fmt);
  vsnprintf(work, len+1, fmt, args);
  va_end(args);
  return work;
}

static int _show_(int erc)
{
  printf("%s\n", _errors_[erc]);
  return erc; 
}

static void _putc_(int ch, FILE *proc)
{
  char buffer[128];
  LOG(_format_(buffer, sizeof(buffer), "%02x", ch));
  putc(ch, proc);
}

static char _putcode_(int* erc, const char* p, FILE *proc)
{
  char work[3];
  work[0] = tolower(p[0]);
  work[1] = tolower(p[1]);
  work[2] = 0;
  if (strspn(work, "0123456789abcdef") != 2)
  {
    *erc = ERC_CODE_IS_NOT_VALID_HEX;
    return;
  }
  unsigned char code;
  if (work[0] >= '0' && work[0] <= '9')
    work[0] -= '0';
  else 
    work[0] = (work[0] - 'a') + 10;  
  if (work[1] >= '0' && work[1] <= '9')
    work[1] -= '0';
  else 
    work[1] = (work[1] - 'a') + 10;  
  code = (unsigned char) (work[0] * 16 + work[1]);
  _putc_(code, proc);
}

static int _already_started_;
static int _started_;

static void _end_screen_()
{
  if (_already_started_ == 0)
    _already_started_ = (stdscr != NULL) ? 1 : -1;
  if (_already_started_ == -1)
    endwin();
  _started_ = 0;
}

static void _start_screen_()
{
  int i;
  if (_already_started_ == 0)
    _already_started_ = (stdscr != NULL) ? 1 : -1;
  if (_started_ == 0)
  {
    if (_already_started_ == -1)
      initscr();
    start_color();
    init_color(COLOR_RED, 1000, 0, 0);
    init_color(COLOR_GREEN, 0, 1000, 0);
    init_color(COLOR_BLUE, 0, 0, 1000);
    for (i = 0; i < 8; i++)
      init_pair(i, i, i > 0 ? 0 : 7);
    cbreak();
    noecho();
    refresh();
    _started_ = 1;
  }
}

static void _submit_(int *erc, const char* program, const char* codes)
{
  if (*erc != 0) return;  
  if (access(program, X_OK) != 0)
  {
    printf("Testing %s: ", program);
    *erc = ERC_PROGRAM_NOT_EXECUTABLE;
    return;
  }
  _start_screen_();
  FILE *proc = popen(program, "w");
  if (proc == 0)
  {
    printf("Running %s: ", program);
    *erc = ERC_PROGRAM_NOT_EXECUTING;
    return;
  }
  setvbuf(proc, NULL, _IONBF, 1);
  const char *p = codes;
  while (*p != 0)
  {
    if (*p == '/')
    {
      if (p[1] == 0) break;
      if (p[1] == '/')
      {
        _putc_(p[1], proc);
        p += 2;
        continue;
      }
      if (p[2] == 0) break;
      _putcode_(erc, p+1, proc);
      if (*erc != 0) break;
      p += 3;
      continue;
    }
    _putc_(p[0], proc);
    p++;
  }
  int rc = pclose(proc);
  _end_screen_();
  if (rc != 0)
  {
    printf("RC %d: ", rc);
    *erc = ERC_PROGRAM_CLOSED_WITH_RC;
  }
}

int main(int argc, char *argv[])
{
  int erc = 0;
  if (argc < 3)
    return(ERC_MISSING_PROGRAM_CODES);
  _submit_(&erc, argv[1], argv[2]);
  return _show_(erc);
}
