000100091016     ***************************************************************************
000200091016     ** Qp0zLprintf()--Print Formatted Job Log Data
000300091016     **
000400091016     ** The Qp0zLprintf() function prints user data specified by format-string
000500091016     ** as an information message type to the job log.
000600091016     **
000700091016     ** Return Value:
000800091016     **
000900091016     ** value Qp0zLprintf() was successful.
001000091016     **       The value returned is the number of characters successfully printed.
001100091016     ** -1    Qp0zLprintf() was not successful.
001200091016     **       The errno variable is set to indicate the error.
001300091016     **
001400091016     ***************************************************************************
001500091016     D PrintFormattedJobLogData...
001600091016     D                 PR            10I 0 EXTPROC('Qp0zLprintf')
001700091016     D  userDataSpecifiedByFormatString...
001800091016     D                                 *   VALUE OPTIONS(*STRING)
001900091016     D  argument1                      *   VALUE OPTIONS(*STRING:*NOPASS)
002000091016     D  argument2                      *   VALUE OPTIONS(*STRING:*NOPASS)
002100091016     D  argument3                      *   VALUE OPTIONS(*STRING:*NOPASS)
002200091016     D  argument4                      *   VALUE OPTIONS(*STRING:*NOPASS)
002300091016     D  argument5                      *   VALUE OPTIONS(*STRING:*NOPASS)
002400091016     D  argument6                      *   VALUE OPTIONS(*STRING:*NOPASS)
002500091016     D  argument7                      *   VALUE OPTIONS(*STRING:*NOPASS)
002600091016     D  argument8                      *   VALUE OPTIONS(*STRING:*NOPASS)
002700091016     D  argument9                      *   VALUE OPTIONS(*STRING:*NOPASS)
002800091016     D  argument10                     *   VALUE OPTIONS(*STRING:*NOPASS)
