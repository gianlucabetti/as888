000100010925      *PARMS DYNUSRPRF(*OWNER)
000200130307      /TITLE Controllo attività PDFSERVER
000300130307     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000400990907
000500990907
000600000710     D* variabili di work
000700060316     D  cntDaElab      s              6  0
000800000710
000900080526
001000080526
001100080526     C*
001200080526     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
001300080526     C
001400080526     C/EXEC SQL
001500080526     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
001600080526     C/END-EXEC
001700130307     C*
001800130307     C* Se check PDFSERVER già fallito invio subito email
001900130307     C                   if        Esito <> *zeros
002000130307     C                   exsr      sndeml
002100130307     C*
002200130307     C* Altrimento verifico cmq che le stampe stiano andando
002300130307     C                   else
002400040309     C*
002500130307     C* Esegue conteggio dei PDF ancora da elaborare
002600040309     C                   exsr      contaDaElab
002700040309     C*
002800130307     C* Esegue considerazioni dei conteggi dei PDF da elaborare
002900130307     C                   if        CntDaElab > NLog0
003000040309     C                   exsr      sndeml
003100040309     C                   endif
003200130307     C                   endif
003300991105     C*
003400040309     C                   seton                                        lr
003500000714
003600000714
003700000714
003800040309      /TITLE Routine di conteggio transazioni ancora da elaborare
003900000714     C*
004000040309     C     contaDaElab   begsr
004100040309     C*
004200040309     C                   eval      cntDaElab = *zeros
004300000714     C*
004400130307     C* Conto quanti rcd di log da elaborare ci sono più vecchi di 5 minuti
004500000714     C/EXEC SQL
004600040309     C+ declare C1 cursor for select count(*)
004700130307     C+ from pdflist where PDFPRTFST = ' ' and
004800130404     C+ minute(current_time - PDFINSTM) > :upl
004900120123     C+ for fetch only
005000000714     C/END-EXEC
005100000714     C
005200000714     C/EXEC SQL
005300040309     C+ open C1
005400000714     C/END-EXEC
005500000714     C
005600000714     C/EXEC SQL
005700040309     C+ Fetch C1 into :cntDaElab
005800000714     C/END-EXEC
005900120123     C
006000000724     C/EXEC SQL
006100040309     C+ close C1
006200000724     C/END-EXEC
006300000714     C*
006400000714     C                   endsr
006500060316
006600060316
006700040309
006800040309
006900040309      /TITLE Compongo il testo e spedisco una e-m@ail
007000040309     C     sndeml        begsr
007100040309     C*
007200040309     C* Inizializzo variabili
007300040309     C                   movel     *blanks       wrkEml          253
007400040309     C                   movel     *blanks       wrkMsg         5000
007500040309     C                   movel     *blanks       wrkOgg           44
007600040309     C* Valorizzo i campi della e-m@ail
007700120305     C                   eval      wrkEml='cedalert@brt.it'
007800130307     C                   eval      wrkOgg='PDFSERVER - WARNING!!!'
007900040309     C                   eval      wrkMsg='ATTENZIONE: '               +':/N'+
008000130307     C                             'i PDF da stampare creati più di '  +
008100120124     C                             %editw(upl: '    ')                 +
008200120123     C                             ' minuti fa, sono: '                +
008300120123     C                             %editw(CntDaElab:'      ') + ' '    +':/N'+
008400130307     C                             'Verificare corretta attività '     +
008500130309     C                             'sottosistema PDFSERVER su AS/400'  +':/N'
008600040309     C*
008700040309     C                   call(e)   'TIS701C'
008800040309     C                   parm                    wrkEml
008900040309     C                   parm                    wrkOgg
009000040309     C                   parm                    wrkMsg
009100040309     C*
009200040309     C                   endsr
009300000714
009400000714
009500000714      /TITLE Routine di *init PGM
009600000714     C*
009700000714     C     *inzsr        begsr
009800000714     C*
009900120123     C     *entry        plist
010000120124     C                   parm                    uplalf            4
010100120124     C                   parm                    NLog0Alfa         4
010200130307     C                   parm                    Esito             1
010300120123     C*
010400120124     C                   move(p)   uplalf        upl               4 0
010500120124     C                   move(p)   NLog0Alfa     NLog0             4 0
010600120123     C*
010700000714     C                   endsr
010800000714
