000001000000     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')
000002000000     H COPYRIGHT('Giuseppe Costagliola 2013 - beppecosta@yahoo.it')
000003000000
000004000000     FSAVF      IF   F  528        Disk
000005000000
000006000000     D CPYTOSAVF       PR
000007000000     D  pSAVF                              like(sSavf)
000008000000     D CPYTOSAVF       PI
000009000000     D  pSAVF                              like(sSavf)
000010000000
000011000000     D sSavf           S             60A
000012000000     D vSavf           S             60A   varying
000013000000
000014000000     D DATA            DS           528
000015000000
000016000000     D openSavf        PR            10I 0 ExtProc('open')
000017000000     D  filename                       *   value options(*string)
000018000000     D  openflags                    10I 0 value
000019000000     D  mode                         10U 0 value options(*nopass)
000020000000     D  codepage                     10U 0 value options(*nopass)
000021000000     D writeSavf       PR            10I 0 ExtProc('write')
000022000000     D  handle                       10i 0 value
000023000000     D  buffer                         *   value
000024000000     D  bytes                        10U 0 value
000025000000     D closeSavf       PR            10I 0 ExtProc('close')
000026000000     D  handle                       10I 0 value
000027000000
000028000000     D O_CREAT         S             10I 0 INZ(8)
000029000000     D O_WRONLY        S             10I 0 INZ(2)
000030000000     D O_TRUNC         S             10I 0 INZ(64)
000031000000     D O_CODEPAGE      S             10I 0 INZ(8388608)
000032000000     D S_IRWXU         S             10I 0 INZ(448)
000033000000     D S_IROTH         S             10I 0 INZ(4)
000034000000
000035000000     D Cpybla          pr                  ExtProc('cpybla')
000036000000     D  Receiver                       *   value
000037000000     D  Source                         *   value
000038000000     D  Size                         10i 0 value
000039000000
000040000000     D oSavf           S             10I 0
000041000000     D bSavf           S               *
000042000000
000043000000     D rc              S             10I 0
000044000000
000045000000      /FREE
000046000000
000047000000       //?Get parms
000048000000       sSavf = %trimR(psavf) + x'00';
000049000000       vSavf = %trimR(sSavf) + x'00';
000050000000
000051000000       //?Allocate buffer for SAVF
000052000000       bSavf = %alloc(528);
000053000000
000054000000       //?Create SAVF
000055000000       oSavF = openSavf(vSavf :
000056000000                   O_CREAT + O_WRONLY + O_TRUNC + O_CODEPAGE :
000057000000                   S_IRWXU + S_IROTH : 819);
000058000000       If oSavf = -1;
000059000000         *INLR = *ON;
000060000000         Return;
000061000000       EndIf;
000062000000
000063000000       //?Write SAVF
000064000000       Read SAVF DATA;
000065000000       Dow not %eof;
000066000000         cpybla (bSavf: %addr(DATA): 528);
000067000000         rc = writeSavf(oSavf:bsavf:528);
000068000000         Read SAVF DATA;
000069000000       EndDo;
000070000000
000071000000       //?close SAVF
000072000000       closeSavf (oSavf);
000073000000
000074000000       *INLR = *ON;
000075000000
000076000000      /END-FREE
000077000000
