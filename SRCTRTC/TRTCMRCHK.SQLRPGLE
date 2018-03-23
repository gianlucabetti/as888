000100010925      *PARMS DYNUSRPRF(*OWNER)
000200060308      /TITLE Controllo attività smaltimento Bart-Mailing
000300990908     H dftactgrp(*yes)
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
001700040309     C*
001800040309     C* Esegue conteggio delle transazioni ancora da elaborare
001900040309     C                   exsr      contaDaElab
002000040309     C*
002100040309     C* Esegue considerazioni dei conteggi transazioni da elaborare sulla base
002200120123     C* del numero log minimi passati come parametro
002300120123     C                   if        CntDaElab > NLog0
002400040309     C                   exsr      sndeml
002500040309     C                   endif
002600991105     C*
002700040309     C                   seton                                        lr
002800000714
002900000714
003000000714
003100040309      /TITLE Routine di conteggio transazioni ancora da elaborare
003200000714     C*
003300040309     C     contaDaElab   begsr
003400040309     C*
003500040309     C                   eval      cntDaElab = *zeros
003600000714     C*
003700120123     C* conto quanti rcd di log a stato 0 e creati X minuti (parametro in ingresso) prima di
003800120123     C* adesso, ci sono ancora
003900000714     C/EXEC SQL
004000040309     C+ declare C1 cursor for select count(*)
004100060308     C+ from timal00f where malsts = '0'
004200120123     C+ and timestampdiff ( 4 ,
004300120123     C+  cast(current_timestamp - (timestamp(maldaor)) as char(22))) > :upl
004400120123     C+ for fetch only
004500000714     C/END-EXEC
004600000714     C
004700000714     C/EXEC SQL
004800040309     C+ open C1
004900000714     C/END-EXEC
005000000714     C
005100000714     C/EXEC SQL
005200040309     C+ Fetch C1 into :cntDaElab
005300000714     C/END-EXEC
005400120123     C
005500000724     C/EXEC SQL
005600040309     C+ close C1
005700000724     C/END-EXEC
005800000714     C*
005900000714     C                   endsr
006000060316
006100060316
006200040309
006300040309
006400040309      /TITLE Compongo il testo e spedisco una e-m@ail
006500040309     C     sndeml        begsr
006600040309     C*
006700040309     C* Inizializzo variabili
006800040309     C                   movel     *blanks       wrkEml          253
006900040309     C                   movel     *blanks       wrkMsg         5000
007000040309     C                   movel     *blanks       wrkOgg           44
007100040309     C* Valorizzo i campi della e-m@ail
007200120305     C                   eval      wrkEml='cedalert@brt.it'
007300060308     C                   eval      wrkOgg='BARTMAIL - WARNING!!!'
007400040309     C                   eval      wrkMsg='ATTENZIONE: '               +':/N'+
007500120123     C                             'il numero di log a stato 0, '      +
007600120123     C                             'creati più di '                    +
007700120124     C                             %editw(upl: '    ')                 +
007800120123     C                             ' minuti fa, sono: '                +
007900120123     C                             %editw(CntDaElab:'      ') + ' '    +':/N'+
008000060308     C                             'Verificare corretta attività su'   +
008100080916     C                             ' CEDBARTMAIL*'                     +':/N'
008200040309     C*
008300040309     C                   call(e)   'TIS701C'
008400040309     C                   parm                    wrkEml
008500040309     C                   parm                    wrkOgg
008600040309     C                   parm                    wrkMsg
008700040309     C*
008800040309     C                   endsr
008900000714
009000000714
009100000714      /TITLE Routine di *init PGM
009200000714     C*
009300000714     C     *inzsr        begsr
009400000714     C*
009500120123     C     *entry        plist
009600120124     C                   parm                    uplalf            4
009700120124     C                   parm                    NLog0Alfa         4
009800120123     C*
009900120124     C                   move(p)   uplalf        upl               4 0
010000120124     C                   move(p)   NLog0Alfa     NLog0             4 0
010100120123     C*
010200000714     C                   endsr
010300000714
