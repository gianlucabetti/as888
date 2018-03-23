000100110617      *---------------------------------------------------------------*
000200130318      * Trattamento DDT PDF pre-stampa - check file                   *
000300110617      *---------------------------------------------------------------*
000400110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500130308     h dftactgrp(*no) actgrp(*caller)
000600130318     h bnddir('QC2LE')
000700110617      *---------------------------------------------------------------*
000800130318
000900130318     Fedivat0f  IF   E             DISK
001000130318
001100130318
001200130318     D*--------------------------------------------------------------------
001300130318     D* Apre una directory
001400130318     D*
001500130318     D* opendir(string af a dirname) ==> return memory *address of the directory
001600130318     D*--------------------------------------------------------------------
001700130318     D opendir         PR              *   EXTPROC('opendir')
001800130318     D                                 *   VALUE
001900130318     D*--------------------------------------------------------------------
002000130318
002100130318     D*--------------------------------------------------------------------
002200130318     D* Chiude una directory
002300130318     D*
002400130318     D* closedir(string af a dirname) ==> return a value of operation's result
002500130318     D*--------------------------------------------------------------------
002600130318     D closedir        PR            10I 0 EXTPROC('closedir')
002700130318     D                                 *   VALUE
002800130318     D*--------------------------------------------------------------------
002900130318
003000130318     D*--------------------------------------------------------------------
003100130318     D* Legge una directory
003200130318     D*
003300130318     D* readdir(*address of a dirname) ==> rtn list main file attrib.into dsinfo
003400130318     D*--------------------------------------------------------------------
003500130318     D readdir         PR              *   EXTPROC('readdir')
003600130318     D                                 *   VALUE
003700130318     D*--------------------------------------------------------------------
003800130318
003900130318     D pPathOrig       s            120A
004000130318     D pPathCli        s                   like(你R8PPDFP)
004100130318     D pNomePDF        s                   like(你R8PPDFN)
004200130318     D pFileName       s            256A
004300130318
004400130318     D dar8吠df      e ds
004500130318     D wPathName       s            256A   inz(*blanks)
004600130318
004700130318     D wDiraddr        s               *
004800130318     D wFiladdr        s               *
004900130318     D wRv             s             10I 0
005000130318
005100130318     D wDirent         ds                  based(wFiladdr)
005200130318     D   d_reserv1                   16A
005300130318     D   d_reserv2                   10U 0
005400130318     D   d_fileno                    10U 0
005500130318     D   d_reclen                    10U 0
005600130318     D   d_reserv3                   10I 0
005700130318     D   d_reserv4                    8A
005800130318     D   d_nlsinfo                   12A
005900130318     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
006000130318     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
006100130318     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
006200130318     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
006300130318     D   d_namelen                   10U 0
006400130318     D   d_name                     640A
006500120619
006600110617
006700110617
006800130318     C
006900130318     C                   eval      pPathOrig =
007000130318     C                             '/QNTC/BARTVAS3/BARTVAS3/BARTFTP/RS400'
007100130318     C                   eval      pPathCli  = '1020001'
007200130318     C                   eval      pNomePDF  = *blanks
007300130318     C*
007400130318     C* Inizializzo i parametri di output
007500130318     C                   eval      pFileName = *blanks
007600130318     C*
007700130318     C* Apro la directory/maschera-file da ricercare
007800130318     C                   eval      wPathName =  %trim(pPathOrig) + '/' +
007900130318     C                                          %trim(pPathCli)  + '/' +
008000130318     C                                          %trim(pNomePDF)  + x'00'
008100130318     C                   eval      wDiraddr  = opendir(%addr(wPathName))
008200130318     C                   if        wDiraddr  = *NULL
008300130318     C*
008400130318     C                   else
008500130318     C*
008600130318     C* Se OK procedo con la lettura di tutti i file che corrispondono alla
008700130318     C* maschera file ricevuta in input - nella cartella ricevuta in input
008800130318     C                   eval      wFiladdr = readdir(wDiraddr)
008900130318     C*
009000130318     C     *start        setll     edivat0f
009100130318     C                   eval      VATTRC = *blanks
009200130318     C                   dow       VATTRC <> 'P' and not %eof(edivat0f)
009300130318     C                   read      edivat0f
009400130318     C                   enddo
009500130318     C*
009600130318     C                   dow       wFiladdr <> *NULL AND
009700130318     C                             not %eof(edivat0f)
009800130318     C*
009900130318     C* Reperisco il nome completo del file corrente
010000130318     C                   eval      pFileName = %subst(d_name:1:d_namelen)
010100130318     C*
010200130318     C* Elaboro solo se trattasi di PDF
010300130318     C                   if        %scan('.pdf':pFileName:1) > 0
010400130318     C                   eval      你R8PPDFN = VATNOT
010500130318     C                   eval      wPathNomeSrc = %trim(pPathOrig) + '/' +
010600130318     C                                            %trim(pPathCli)  + '/' +
010700130318     C                                            %trim(pFileName)
010800130318     C                   eval      wPathNomeDst = %trim(你R8PPDFN)
010900130318     C                   call      'TNVR_FG02'
011000130318     C                   parm                    wPathNomeSrc    256
011100130318     C                   parm                    wPathNomeDst    256
011200130318     C*
011300130318     C                   eval      VATTRC = *blanks
011400130318     C                   dow       VATTRC <> 'P' and not %eof(edivat0f)
011500130318     C                   read      edivat0f
011600130318     C                   enddo
011700130318     C                   endif
011800130318     C*
011900130318     C* Procedo con la lettura di tutti i file che corrispondono alla
012000130318     C* maschera file ricevuta in input - nella cartella ricevuta in input
012100130318     C                   eval      wFiladdr = readdir(wDiraddr)
012200130318     C*
012300130318     C                   enddo
012400130318     C*
012500130318     C* Chiudo la directory/maschera-file da ricercare
012600130318     C                   eval      wRv = closedir(wDiraddr)
012700130318     C                   endif
012800130318     C*
012900130318     C                   seton                                        LR
013000130318     C
