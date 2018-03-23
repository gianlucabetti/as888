000100040309      /TITLE Controllo attività UpLoad
000200990908     H dftactgrp(*yes)
000300160312     H DECEDIT('0,') DATEDIT(*DMY.)
000400990907
000500990907
000600160312     D* Variabili di work
000700160312     D  wSqlKSC        s              8    inz
000800160312     D  wSqlTIP        s              2    inz
000900160312     D  wSqlTOTREC     s             11  0 inz
001000160312     D  wSqlDELLOAD    s              8  0 inz
001100160312     D  wSqlDATLOAD    s              8  0 inz
001200160312     D  wMessaggio     s           5000    inz
001300160312
001400160312
001500000710
001600120305     C*
001700120305     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
001800120305     C
001900120305     C/EXEC SQL
002000160317     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
002100120305     C/END-EXEC
002200160312     C*
002300160312     C* Esegue verifica overload dati in upload
002400160312     C                   exsr      ChkOverload
002500160312     C*
002600160317     C                   seton                                        lr
002700000714
002800160312
002900160312
003000160312      /TITLE Routine di verifica overload dati in upload
003100160312     C*
003200160312     C     ChkOverload   begsr
003300160312     C*
003400160312     C/EXEC SQL
003500160312     C+ declare C2 cursor for
003600160312     C+ with
003700160312     C+ SEL_DATLOAD as (
003800160312     C+ select decimal(trim(substr(VIREFLO, 1, 8)), 8, 0) as DATLOAD
003900160312     C+ from tivire0f where vireksc='*DATLOAD' and viretip='**' and
004000160312     C+ viredti = 0 and viretrc='VA'),
004100160312     C+ SEL_MAXLOAD as (
004200160312     C+ select decimal(trim(substr(VIREFLO, 1, 8)), 8, 0) as MAXLOAD
004300160312     C+ from tivire0f where vireksc='*MAXLOAD' and viretip='**' and
004400160312     C+ viredti = 0 and viretrc='VA'),
004500160312     C+ SEL_DELLOAD as (
004600160312     C+ select decimal(trim(substr(VIREFLO, 1, 8)), 8, 0) as DELLOAD
004700160312     C+ from tivire0f where vireksc='*DELLOAD' and viretip='**' and
004800160312     C+ viredti = 0 and viretrc='VA'),
004900160312     C+ SEL_VIN_001 as (
005000160312     C+ SELECT VINKSC, VINTIP, VINDAT
005100160312     C+ FROM TIVIN00S where vindat >= (select DATLOAD from SEL_DATLOAD))
005200160312     C+ SELECT
005300160312     C+ VINKSC, VINTIP, DECIMAL(COUNT(*), 11, 0) AS TOTREC,
005400160312     C+ min(DELLOAD), min(DATLOAD)
005500160312     C+ FROM SEL_VIN_001, SEL_DELLOAD, SEL_DATLOAD
005600160312     C+ GROUP BY VINKSC, VINTIP
005700160312     C+ HAVING count(*) > (select MAXLOAD from SEL_MAXLOAD)
005800160317     C+ for read only
005900160312     C/END-EXEC
006000160312     C
006100160312     C/EXEC SQL
006200160312     C+ open C2
006300160312     C/END-EXEC
006400160312     C
006500160312     C/EXEC SQL
006600160312     C+ Fetch C2 into :wSqlKSC, :wSqlTIP, :wSqlTOTREC,
006700160312     C+               :wSqlDELLOAD, :wSqlDATLOAD
006800160312     C/END-EXEC
006900160312     C*
007000160312     C                   dow       sqlcod = *zeros
007100160312     C*
007200160312     C* Per ogni record invio email segnalando che per il cliente/tipo-file corrente
007300160312     C* si è superata soglia warning di quantità dati inviati
007400160312     C                   eval      wMessaggio='ATTENZIONE: '           +':/N'+
007500160312     C                             'il cliente ' + wSqlKSC + ' per il '+
007600160312     C                             'tipo file ' + wSqlTIP + ' ha già inviato '+
007700160312     C                             %trim(%editc(wSqlTOTREC:'1')) +
007800160312     C                             ' records '+
007900160312     C                             'dal '+%char(wSqlDATLOAD)+'.'+':/N'+
008000160312     C                             'Tenere strettamente monitorato.'
008100160312     C                   exsr      sndeml
008200160312     C*
008300160312     C* Per ogni record che supera la soglia massima =>
008400160314     C*  - faccio scadere anagrafica scambio dati
008500160314     C*  - si eliminano i records
008600160312     C*  - si invia un email di avviso
008700160312     C                   if        wSqlTOTREC > wSqlDELLOAD
008800160314     C*
008900160314     C/EXEC SQL
009000160314     C+ UPDATE TIVIR00F SET VIRDTF=VIRDTI
009100160314     C+ WHERE VINKSC  = :wSqlKSC AND VINTIP = :wSqlTIP
009200160314     C/END-EXEC
009300160312     C*
009400160312     C/EXEC SQL
009500160312     C+ DELETE FROM TIVIN00S
009600160312     C+ WHERE VINKSC  = :wSqlKSC AND VINTIP = :wSqlTIP AND
009700160312     C+       VINDAT >= :wSqlDATLOAD
009800160312     C/END-EXEC
009900160312     C                   eval      wMessaggio='ATTENZIONE: '           +':/N'+
010000160312     C                             'il cliente ' + wSqlKSC + ' per il '+
010100160312     C                             'tipo file ' + wSqlTIP + ' ha superato '+
010200160312     C                             'la soglia massima di dati inviabili di: '+
010300160312     C                             %trim(%editc(wSqlDELLOAD:'1')) +
010400160312     C                             ' records '+
010500160312     C                             'dal '+%char(wSqlDATLOAD)+'.'+':/N'+
010600160312     C                             'Per sicurezza i dati sono stati eliminati.'+
010700160312     C                             ':/N'+
010800160314     C                             'Per sicurezza la anagrafica di scambio '+
010900160314     C                             'dati è stata disabilitata.'+':/N'+
011000160312     C                             'Verificare immediatamente !!'
011100160312     C                   exsr      sndeml
011200160312     C                   endif
011300160312     C*
011400160312     C/EXEC SQL
011500160312     C+ Fetch C2 into :wSqlKSC, :wSqlTIP, :wSqlTOTREC,
011600160312     C+               :wSqlDELLOAD, :wSqlDATLOAD
011700160312     C/END-EXEC
011800160312     C*
011900160312     C                   enddo
012000160312     C*
012100160312     C/EXEC SQL
012200160312     C+ close C2
012300160312     C/END-EXEC
012400160312     C*
012500160312     C*
012600160312     C                   endsr
012700040309
012800040309
012900040309
013000040309      /TITLE Compongo il testo e spedisco una e-m@ail
013100040309     C     sndeml        begsr
013200040309     C*
013300040309     C* Inizializzo variabili
013400160312     C                   movel     *blanks       wrkEmlTo        253
013500160312     C                   movel     *blanks       wrkEmlCc        253
013600040309     C                   movel     *blanks       wrkMsg         5000
013700040309     C                   movel     *blanks       wrkOgg           44
013800160312     C                   movel     *blanks       wrkEsi            1
013900040309     C* Valorizzo i campi della e-m@ail
014000160312     C                   eval      wrkEmlTo='cedalert@brt.it'
014100171006     C                   eval      wrkEmlCc=*blank
014200040309     C                   eval      wrkOgg='VAS - WARNING!!!'
014300160312     C                   eval      wrkMsg=wMessaggio
014400040309     C*
014500160312     C                   call(e)   'TIS701C1'
014600160312     C                   parm                    wrkEmlTo
014700160312     C                   parm                    wrkEmlCc
014800040309     C                   parm                    wrkOgg
014900040309     C                   parm                    wrkMsg
015000160312     C                   parm                    wrkEsi
015100040309     C*
015200040309     C                   endsr
015300000714
015400000714
015500000714      /TITLE Routine di *init PGM
015600000714     C*
015700000714     C     *inzsr        begsr
015800000714     C*
015900000714     C                   endsr
016000000714
