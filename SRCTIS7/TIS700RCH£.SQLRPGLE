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
001200160312     D  wSqlKSC        s              8    inz
001300160312     D  wSqlTIP        s              2    inz
001400160312     D  wSqlTOTREC     s             11  0 inz
001500160312     D  wSqlDELLOAD    s              8  0 inz
001600160312     D  wSqlDATLOAD    s              8  0 inz
001700160312     D  wMessaggio     s           5000    inz
001800160312
001900160312
002000000710
002100120305     C*
002200120305     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
002300120305     C
002400120305     C/EXEC SQL
002500120305     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
002600120305     C/END-EXEC
002700160312     C*
002800160312     C* Esegue verifica overload dati in upload
002900160312     C                   exsr      ChkOverload
003000160312     C*
003100160312     C* Esegue verifica attività di elaborazione transazioni
003200160312     C                   exsr      ChkActivity
003300160312     C*
003400160312     C                   return
003500160312
003600160312
003700160312
003800160312      /TITLE Routine di verifica attività di elaborazione transazioni
003900160312     C*
004000160312     C     ChkActivity   begsr
004100160312     C*
004200160312     C* Esegue conteggio delle transazioni ancora da elaborare
004300160312     C                   exsr      contaDaElab
004400160312     C*
004500160312     C* Esegue considerazioni dei conteggi transazioni da elaborare sulla base
004600160312     C* d 3 giri distanziati d 5 minuti l'uno dall'altro
004700160312     C                   if        Conta3 <> *zeros AND
004800160312     C                             Conta2 <> *zeros AND
004900160312     C                             Conta1 <> *zeros AND
005000160312     C                             Conta3 >= Conta2 AND
005100160312     C                             Conta2 >= Conta1
005200160312     C* Se le transazioni da elaborare in virtù d quanto sopra nn diminuiscono
005300160312     C* e sono superiori a 20 => mando 1 e-mail a ced x verifica
005400160312     C                   if        Conta1 > 20
005500160312     C                   eval      wMessaggio='ATTENZIONE: '           +':/N'+
005600160312     C                             'il numero di transazioni in upload'+
005700160312     C                             ' da elaborare non diminuisce!'     +':/N'+
005800160312     C                             'Attualmente sono: '                +
005900160312     C                             %editw(Conta3:'      ')             +
006000160312     C                             ' a stato "0" o stato "M".'         +':/N'+
006100160312     C                             'Verificare i job attivi nel sottosistema'+
006200160312     C                             ' STRATEBA7'                        +':/N'
006300160312     C                   exsr      sndeml
006400160312     C                   endif
006500160312     C                   endif
006600160312     C*
006700160312     C                   if        Giro = 3
006800160312     C                   seton                                        lr
006900160312     C                   else
007000160312     C                   return
007100160312     C                   endif
007200160312     C*
007300160312     C                   endsr
007400000714
007500160312
007600160312
007700160312      /TITLE Routine di verifica overload dati in upload
007800160312     C*
007900160312     C     ChkOverload   begsr
008000160312     C*
008100160312     C/EXEC SQL
008200160312     C+ declare C2 cursor for
008300160312     C+ with
008400160312     C+ SEL_DATLOAD as (
008500160312     C+ select decimal(trim(substr(VIREFLO, 1, 8)), 8, 0) as DATLOAD
008600160312     C+ from tivire0f where vireksc='*DATLOAD' and viretip='**' and
008700160312     C+ viredti = 0 and viretrc='VA'),
008800160312     C+ SEL_MAXLOAD as (
008900160312     C+ select decimal(trim(substr(VIREFLO, 1, 8)), 8, 0) as MAXLOAD
009000160312     C+ from tivire0f where vireksc='*MAXLOAD' and viretip='**' and
009100160312     C+ viredti = 0 and viretrc='VA'),
009200160312     C+ SEL_DELLOAD as (
009300160312     C+ select decimal(trim(substr(VIREFLO, 1, 8)), 8, 0) as DELLOAD
009400160312     C+ from tivire0f where vireksc='*DELLOAD' and viretip='**' and
009500160312     C+ viredti = 0 and viretrc='VA'),
009600160312     C+ SEL_VIN_001 as (
009700160312     C+ SELECT VINKSC, VINTIP, VINDAT
009800160312     C+ FROM TIVIN00S where vindat >= (select DATLOAD from SEL_DATLOAD))
009900160312     C+ SELECT
010000160312     C+ VINKSC, VINTIP, DECIMAL(COUNT(*), 11, 0) AS TOTREC,
010100160312     C+ min(DELLOAD), min(DATLOAD)
010200160312     C+ FROM SEL_VIN_001, SEL_DELLOAD, SEL_DATLOAD
010300160312     C+ GROUP BY VINKSC, VINTIP
010400160312     C+ HAVING count(*) > (select MAXLOAD from SEL_MAXLOAD)
010500160312     C+ for fetch only
010600160312     C/END-EXEC
010700160312     C
010800160312     C/EXEC SQL
010900160312     C+ open C2
011000160312     C/END-EXEC
011100160312     C
011200160312     C/EXEC SQL
011300160312     C+ Fetch C2 into :wSqlKSC, :wSqlTIP, :wSqlTOTREC,
011400160312     C+               :wSqlDELLOAD, :wSqlDATLOAD
011500160312     C/END-EXEC
011600160312     C*
011700160312     C                   dow       sqlcod = *zeros
011800160312     C*
011900160312     C* Per ogni record invio email segnalando che per il cliente/tipo-file corrente
012000160312     C* si è superata soglia warning di quantità dati inviati
012100160312     C                   eval      wMessaggio='ATTENZIONE: '           +':/N'+
012200160312     C                             'il cliente ' + wSqlKSC + ' per il '+
012300160312     C                             'tipo file ' + wSqlTIP + ' ha già inviato '+
012400160312     C                             %trim(%editc(wSqlTOTREC:'1')) +
012500160312     C                             ' records '+
012600160312     C                             'dal '+%char(wSqlDATLOAD)+'.'+':/N'+
012700160312     C                             'Tenere strettamente monitorato.'
012800160312     C                   exsr      sndeml
012900160312     C*
013000160312     C* Per ogni record che supera la soglia massima =>
013100160314     C*  - faccio scadere anagrafica scambio dati
013200160314     C*  - si eliminano i records
013300160312     C*  - si invia un email di avviso
013400160312     C                   if        wSqlTOTREC > wSqlDELLOAD
013500160314     C*
013600160314     C/EXEC SQL
013700160314     C+ UPDATE TIVIR00F SET VIRDTF=VIRDTI
013800160314     C+ WHERE VINKSC  = :wSqlKSC AND VINTIP = :wSqlTIP
013900160314     C/END-EXEC
014000160312     C*
014100160312     C/EXEC SQL
014200160312     C+ DELETE FROM TIVIN00S
014300160312     C+ WHERE VINKSC  = :wSqlKSC AND VINTIP = :wSqlTIP AND
014400160312     C+       VINDAT >= :wSqlDATLOAD
014500160312     C/END-EXEC
014600160312     C                   eval      wMessaggio='ATTENZIONE: '           +':/N'+
014700160312     C                             'il cliente ' + wSqlKSC + ' per il '+
014800160312     C                             'tipo file ' + wSqlTIP + ' ha superato '+
014900160312     C                             'la soglia massima di dati inviabili di: '+
015000160312     C                             %trim(%editc(wSqlDELLOAD:'1')) +
015100160312     C                             ' records '+
015200160312     C                             'dal '+%char(wSqlDATLOAD)+'.'+':/N'+
015300160312     C                             'Per sicurezza i dati sono stati eliminati.'+
015400160312     C                             ':/N'+
015500160314     C                             'Per sicurezza la anagrafica di scambio '+
015600160314     C                             'dati è stata disabilitata.'+':/N'+
015700160312     C                             'Verificare immediatamente !!'
015800160312     C                   exsr      sndeml
015900160312     C                   endif
016000160312     C*
016100160312     C/EXEC SQL
016200160312     C+ Fetch C2 into :wSqlKSC, :wSqlTIP, :wSqlTOTREC,
016300160312     C+               :wSqlDELLOAD, :wSqlDATLOAD
016400160312     C/END-EXEC
016500160312     C*
016600160312     C                   enddo
016700160312     C*
016800160312     C/EXEC SQL
016900160312     C+ close C2
017000160312     C/END-EXEC
017100160312     C*
017200160312     C*
017300160312     C                   endsr
017400000714
017500000714
017600000714
017700040309      /TITLE Routine di conteggio transazioni ancora da elaborare
017800000714     C*
017900040309     C     contaDaElab   begsr
018000040309     C*
018100040309     C                   eval      cntDaElab = *zeros
018200000714     C*
018300000714     C/EXEC SQL
018400040309     C+ declare C1 cursor for select count(*)
018500060103     C+ from tivlr00f where vlrflg = '0' or vlrflg = 'M'
018600000714     C+ for fetch only
018700000714     C/END-EXEC
018800000714     C
018900000714     C/EXEC SQL
019000040309     C+ open C1
019100000714     C/END-EXEC
019200000714     C
019300000714     C/EXEC SQL
019400040309     C+ Fetch C1 into :cntDaElab
019500000714     C/END-EXEC
019600000714     C*
019700040309     C                   if        sqlcod = *zeros
019800040309     C                   eval      Giro = Giro + 1
019900040309     C                   if        Giro = 1
020000040309     C                   eval      conta1 = cntDaElab
020100040309     C                   endif
020200040309     C                   if        Giro = 2
020300040309     C                   eval      conta2 = cntDaElab
020400040309     C                   endif
020500040309     C                   if        Giro = 3
020600040309     C                   eval      conta3 = cntDaElab
020700040309     C                   endif
020800000714     C                   endif
020900040309     C*
021000000724     C/EXEC SQL
021100040309     C+ close C1
021200000724     C/END-EXEC
021300000714     C*
021400000714     C                   endsr
021500040309
021600040309
021700040309
021800040309
021900040309      /TITLE Compongo il testo e spedisco una e-m@ail
022000040309     C     sndeml        begsr
022100040309     C*
022200040309     C* Inizializzo variabili
022300160312     C                   movel     *blanks       wrkEmlTo        253
022400160312     C                   movel     *blanks       wrkEmlCc        253
022500040309     C                   movel     *blanks       wrkMsg         5000
022600040309     C                   movel     *blanks       wrkOgg           44
022700160312     C                   movel     *blanks       wrkEsi            1
022800040309     C* Valorizzo i campi della e-m@ail
022900160312     C                   eval      wrkEmlTo='cedalert@brt.it'
023000160312     C                   eval      wrkEmlCc='fabrizio.gurrieri@brt.it'
023100040309     C                   eval      wrkOgg='VAS - WARNING!!!'
023200160312     C                   eval      wrkMsg=wMessaggio
023300040309     C*
023400160312     C                   call(e)   'TIS701C1'
023500160312     C                   parm                    wrkEmlTo
023600160312     C                   parm                    wrkEmlCc
023700040309     C                   parm                    wrkOgg
023800040309     C                   parm                    wrkMsg
023900160312     C                   parm                    wrkEsi
024000040309     C*
024100040309     C                   endsr
024200000714
024300000714
024400000714      /TITLE Routine di *init PGM
024500000714     C*
024600000714     C     *inzsr        begsr
024700000714     C*
024800000714     C                   endsr
024900000714
