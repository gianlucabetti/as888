000001000000     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')
000002000000     H COPYRIGHT('Giuseppe Costagliola 2013 - beppecosta@yahoo.it')
000003000000
000004000000     FSAVF      O    F  528        Disk
000005000000
000006000000     D CPYFRMSAVF      PR
000007000000     D  pSAVF                              like(sSavf)
000008000000     D CPYFRMSAVF      PI
000009000000     D  pSAVF                              like(sSavf)
000010000000
000011000000     D Cpybla          pr                  ExtProc('cpybla')
000012000000     D  Receiver                       *   value
000013000000     D  Source                         *   value
000014000000     D  Size                         10i 0 value
000015000000
000016000000     D DATA            S            528A
000017000000
000018000000     Dstat             PR            10I 0 extproc('stat')
000019000000     Dpathptr                          *   Value options(*String)
000020000000     Dstatptr                          *   value
000021000000
000022000000     D statds          DS                  qualified
000023000000     D  mode                         10U 0
000024000000     D  ino                          10U 0
000025000000     D  nlink                         5U 0
000026000000     D  filler1                       5U 0
000027000000     D  uid                          10U 0
000028000000     D  gid                          10U 0
000029000000     D  size                         10U 0
000030000000     D  filler2                     256
000031000000
000032000000     D sSavf           S             60A
000033000000     D vSavf           S             60A   varying
000034000000
000035000000     D openSavf        PR            10I 0 ExtProc('open')
000036000000     D  path_                          *   Value options(*String)
000037000000     D  oflag                        10I 0 Value
000038000000     D  mode                         10U 0 Value Options(*nopass)
000039000000     D  codepage                     10U 0 Value Options(*nopass)
000040000000     D readSavf        PR            10I 0 Extproc('read')
000041000000     D  FileDescI                    10I 0 Value
000042000000     D  buffer                         *   Value
000043000000     D  nbyte                        10U 0 Value
000044000000
000045000000     D closeSavf       PR            10I 0 Extproc('close')
000046000000     D  FileDesc                     10I 0 Value
000047000000
000048000000     D rc              S             10I 0
000049000000
000050000000     D iSavf           S             10I 0
000051000000     D bSavf           S               *
000052000000
000053000000     D O_RDONLY        S             10I 0 INZ(x'01')
000054000000
000055000000      /FREE
000056000000
000057000000       //?Get parms
000058000000       sSavf = %trimR(psavf) + x'00';
000059000000       vSavf = %trimR(sSavf) + x'00';
000060000000       rc = stat(vSavf:%addr(statds));
000061000000
000062000000       //?Allocate buffer for SAVF
000063000000       bSavf = %alloc(528);
000064000000
000065000000       //?Read SAVF
000066000000       iSavf = openSavf(vSavf: O_RDONLY);
000067000000       Dou rc <= 0;
000068000000         rc = readSavf(iSavf : bSavf : 528);
000069000000         If rc = 528;
000070000000           cpybla (%addr(DATA): bSavf: 528);
000071000000           Except;
000072000000         EndIf;
000073000000       EndDo;
000074000000
000075000000       //?Close savf
000076000000       closeSavf(iSavf);
000077000000       *INLR = *ON;
000078000000
000079000000      /END-FREE
000080000000
000081000000     OSAVF      E
000082000000     O                       DATA               528
