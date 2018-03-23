000100040309      /TITLE Controllo attività UpLoad
000200990908     H dftactgrp(*yes)
000300160312     H DECEDIT('0,') DATEDIT(*DMY.)
000400990907
000500990907
000600160312     D* Variabili di work
000700040309     D  conta1         s              6  0 inz(*zeros)
000800040309     D  conta2         s              6  0 inz(*zeros)
000900040309     D  conta3         s              6  0 inz(*zeros)
001000040309     D  cntDaElab      s              3  0
001100040309     D  Giro           s              1  0 inz(*zeros)
001200160312     D  wMessaggio     s           5000    inz
001300160312
001400160312
001500000710
001600120305     C*
001700120305     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
001800120305     C
001900120305     C/EXEC SQL
002000120305     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
002100120305     C/END-EXEC
002200160312     C*
002300160312     C* Esegue verifica attività di elaborazione transazioni
002400160312     C                   exsr      ChkActivity
002500160312     C*
002600160312     C                   return
002700160312
002800160312
002900160312
003000160312      /TITLE Routine di verifica attività di elaborazione transazioni
003100160312     C*
003200160312     C     ChkActivity   begsr
003300160312     C*
003400160312     C* Esegue conteggio delle transazioni ancora da elaborare
003500160312     C                   exsr      contaDaElab
003600160312     C*
003700160312     C* Esegue considerazioni dei conteggi transazioni da elaborare sulla base
003800160312     C* d 3 giri distanziati d 5 minuti l'uno dall'altro
003900160312     C                   if        Conta3 <> *zeros AND
004000160312     C                             Conta2 <> *zeros AND
004100160312     C                             Conta1 <> *zeros AND
004200160312     C                             Conta3 >= Conta2 AND
004300160312     C                             Conta2 >= Conta1
004400160312     C* Se le transazioni da elaborare in virtù d quanto sopra nn diminuiscono
004500160312     C* e sono superiori a 20 => mando 1 e-mail a ced x verifica
004600160312     C                   if        Conta1 > 20
004700160312     C                   eval      wMessaggio='ATTENZIONE: '           +':/N'+
004800160312     C                             'il numero di transazioni in upload'+
004900160312     C                             ' da elaborare non diminuisce!'     +':/N'+
005000160312     C                             'Attualmente sono: '                +
005100160312     C                             %editw(Conta3:'      ')             +
005200160312     C                             ' a stato "0" o stato "M".'         +':/N'+
005300160312     C                             'Verificare i job attivi nel sottosistema'+
005400160312     C                             ' STRATEBA7'                        +':/N'
005500160312     C                   exsr      sndeml
005600160312     C                   endif
005700160312     C                   endif
005800160312     C*
005900160312     C                   if        Giro = 3
006000160312     C                   seton                                        lr
006100160312     C                   else
006200160312     C                   return
006300160312     C                   endif
006400160312     C*
006500160312     C                   endsr
006600000714
006700000714
006800000714
006900040309      /TITLE Routine di conteggio transazioni ancora da elaborare
007000000714     C*
007100040309     C     contaDaElab   begsr
007200040309     C*
007300040309     C                   eval      cntDaElab = *zeros
007400000714     C*
007500000714     C/EXEC SQL
007600040309     C+ declare C1 cursor for select count(*)
007700060103     C+ from tivlr00f where vlrflg = '0' or vlrflg = 'M'
007800000714     C+ for fetch only
007900000714     C/END-EXEC
008000000714     C
008100000714     C/EXEC SQL
008200040309     C+ open C1
008300000714     C/END-EXEC
008400000714     C
008500000714     C/EXEC SQL
008600040309     C+ Fetch C1 into :cntDaElab
008700000714     C/END-EXEC
008800000714     C*
008900040309     C                   if        sqlcod = *zeros
009000040309     C                   eval      Giro = Giro + 1
009100040309     C                   if        Giro = 1
009200040309     C                   eval      conta1 = cntDaElab
009300040309     C                   endif
009400040309     C                   if        Giro = 2
009500040309     C                   eval      conta2 = cntDaElab
009600040309     C                   endif
009700040309     C                   if        Giro = 3
009800040309     C                   eval      conta3 = cntDaElab
009900040309     C                   endif
010000000714     C                   endif
010100040309     C*
010200000724     C/EXEC SQL
010300040309     C+ close C1
010400000724     C/END-EXEC
010500000714     C*
010600000714     C                   endsr
010700040309
010800040309
010900040309
011000040309
011100040309      /TITLE Compongo il testo e spedisco una e-m@ail
011200040309     C     sndeml        begsr
011300040309     C*
011400040309     C* Inizializzo variabili
011500160312     C                   movel     *blanks       wrkEmlTo        253
011600160312     C                   movel     *blanks       wrkEmlCc        253
011700040309     C                   movel     *blanks       wrkMsg         5000
011800040309     C                   movel     *blanks       wrkOgg           44
011900160312     C                   movel     *blanks       wrkEsi            1
012000040309     C* Valorizzo i campi della e-m@ail
012100160312     C                   eval      wrkEmlTo='cedalert@brt.it'
012200171006     C                   eval      wrkEmlCc=*blank
012300040309     C                   eval      wrkOgg='VAS - WARNING!!!'
012400160312     C                   eval      wrkMsg=wMessaggio
012500040309     C*
012600160312     C                   call(e)   'TIS701C1'
012700160312     C                   parm                    wrkEmlTo
012800160312     C                   parm                    wrkEmlCc
012900040309     C                   parm                    wrkOgg
013000040309     C                   parm                    wrkMsg
013100160312     C                   parm                    wrkEsi
013200040309     C*
013300040309     C                   endsr
013400000714
013500000714
013600000714      /TITLE Routine di *init PGM
013700000714     C*
013800000714     C     *inzsr        begsr
013900000714     C*
014000000714     C                   endsr
014100000714
