000100000000     H*------------------------------------------------------------------------*
000200071228     H* CREAZIONE FILE DATI CLIENTI/FORNITORI PER POSTEL
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000914     H*------------------------------------------------------------------------*
000600000925     FAZORG01L  IF   E           K DISK
000700081120     FWFPTL10F  IF A E           K DISK    USROPN
000800131003     fAZNTC01L  if   e           k disk    prefix(AZ)
000900020925     FTFNTC01L  IF   E           K DISK
001000081120     FTncpo01L  IF   E           K DISK
001100000000     D*------------------------------------------------------------------------*
001200000000     D* SCHIERE
001300000000     D*------------------------------------------------------------------------*
001400000000     D*------------------------------------------------------------------------*
001500000000     D* INPUT
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000000     D* ARCHITETTURA
001900000000     D*-------------------
002000000000     D KPJBA         E DS
002100000914     D*------------------------------------------------------------------------*
002200000918     D* VARIABILI DI WRK
002300000914     D*------------------------------------------------------------------------*
002400081120     d command         S             80
002500081120     d piva            s             16
002600081124     d savpiv          s             16
002700081120     d T_nota          s              2
002800081120     d nota_ok         s              1
002900081120     d codcpo          s                   like(cpocpo)
003000081120
003100081120     d KNTapl          s                   like(ntcapl) inz('P')
003200081120     d KNTnk1          s                   like(ntcnk1)
003300081120     d KNTnk2          s                   like(ntcnk2)
003400081120     d KNTtnt          s                   like(ntctnt)
003500081120     D*------------------
003600000921     D* DS "XSRDA8" - CONTROLLA DATA (8)
003700000921     D*------------------
003800000921     D WLBDA8          DS                  INZ
003900000921     D  G08DAT                 1      8  0
004000000921     D  G08INV                 9     16  0
004100000921     D  G08ERR                17     17
004200000921     D  G08TGI                18     22  0
004300081124     D*------------------
004400081124     D* DS DI WRK
004500081124     D*------------------
004600081124     D DS_cpnot        DS
004700081124     D  §_cpopiv                     16
004800081124     D  §_cpocpo                     11  0
004900081124     D  §_cpnok1                     15  0
005000081124     D  §_cpnopr                      2
005100081124     D  §_cpnotn                      2
005200081124     D  §_cpnorn                     60
005300020923     D*-------------------
005400020923     D* DS REPERIMENTO DATI UTENTE
005500020923     D*-------------------
005600020923     D TIBS34DS      E DS                                                       *Profili utente
005700020923     D DDATIUTE      E DS                                                       *Dati utente
005800020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
005900000000     C*------------------------------------------------------------------------*
006000000000     C* MAIN LINE
006100000000     C*------------------------------------------------------------------------*
006200071212     C*
006300071212     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006400071212     C
006500071212     C/EXEC SQL
006600071212     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006700071212     C/END-EXEC
006800000000     C*
006900000920     C                   exsr      procedi
007000050404     C*
007100000914     C*
007200000914     C                   SETON                                        LR
007300000922     C*------------------------------------------------------------------------*
007400000922     C* PROCEDI - Routine principale
007500000922     C*------------------------------------------------------------------------*
007600000922     C     PROCEDI       BEGSR
007700000922     C*
007800081120     C* CLRPFM del file (WFPTL10F)
007900030306     C                   Z-ADD     80            LENGH            15 5
008000081120     c                   eval      command = 'CLRPFM FILE(WFPTL10F)'
008100081124     C                   CALL      'QCMDEXC'                            60      *LANCIA CMD
008200081120     C                   PARM                    Command
008300030307     C                   PARM                    LENGH
008400071231     C*
008500081120     C* Apro il file (WFPTL10F)
008600081120     C                   open      wfptl10f
008700000922     C*
008800000922     C* Se riuscito comando ==> procedo
008900081124     C                   if        not *in60
009000081120     C                   exsr      exepotenz
009100081120     c                   endif
009200071231     C*
009300081120     C* Chiudo il file (WFPTL10F)
009400081120     C                   close     wfptl10f
009500000922     C*
009600000922     C                   ENDSR
009700000914     C*------------------------------------------------------------------------*
009800081120     C* EXEPOTENZ - Routine di estrazione potenziali
009900000914     C*------------------------------------------------------------------------*
010000081120     C     EXEPOTENZ     BEGSR
010100000914     C*
010200081121
010300081121     C/EXEC SQL
010400081121     C+ CREATE TABLE QTEMP/IVA_CPO  (
010500081121     C+ §_CPOPIV CHAR (16) NOT NULL WITH DEFAULT,
010600081121     C+ §_CPOCPO DEC (11, 0) NOT NULL WITH DEFAULT)
010700081121     C/END-EXEC
010800081121
010900000914     C/EXEC SQL
011000081121     C+ INSERT INTO QTEMP/IVA_CPO SELECT
011100081121     C+ distinct cpopiv, CPOCPO
011200081121     C+ from tfntc00f joIn tncpo06l
011300081120     C+ on dec(ntcnk1) = cpocpo where
011400081120     C+ ntcapl = 'P' and CPOpiv <> ' '  and
011500081120     C+ ntctnt in ('06', '03', '88', '85',
011600081120     C+ '84', '87', '08') and ntcflt <> 'A'
011700081120     C+ and cpopiv not in (select indiva from cnind00f)
011800000914     C/END-EXEC
011900000914     C
012000081121     C/EXEC SQL
012100081121     C+ CREATE TABLE QTEMP/IVA_CPONOT (
012200081121     C+ §_CPNOK1 DEC (11, 0) NOT NULL WITH DEFAULT,
012300081121     C+ §_CPNOPR CHAR (2) NOT NULL WITH DEFAULT,
012400081121     C+ §_CPNOTN CHAR (2) NOT NULL WITH DEFAULT,
012500081121     C+ §_CPNORN CHAR (60) NOT NULL WITH DEFAULT)
012600081121     C/END-EXEC
012700081121
012800081121
012900081121     C/EXEC SQL
013000081121     C+ INSERT INTO QTEMP/IVA_CPONOT SELECT
013100081121     C+ dec(ntcnk1), '01', ntctnt, ntcrnt
013200081121     C+ from tfntc00f
013300081121     C+ where ntctnt = '06' and
013400081124     C+ ntcflt<>'A' and ntcapl = 'P'
013500081124     C+ and dec(ntcnk1) in (select §_cpocpo from
013600081121     C+ QTEMP/iva_cpo)
013700081121     C+ union
013800081121     C+ select dec(ntcnk1), '02', ntctnt, ntcrnt
013900081121     C+ from tfntc00f
014000081121     C+ where ntctnt = '03' and
014100081124     C+ ntcflt<>'A'  and ntcapl = 'P'
014200081124     C+ and dec(ntcnk1) in (select §_cpocpo from
014300081121     C+ QTEMP/iva_cpo)
014400081121     C+ union
014500081121     C+ select dec(ntcnk1), '03', ntctnt, ntcrnt
014600081121     C+ from tfntc00f
014700081121     C+ where ntctnt = '88' and
014800081124     C+ ntcflt<>'A' and ntcapl = 'P'
014900081124     C+ and dec(ntcnk1) in (select §_cpocpo from
015000081121     C+ QTEMP/iva_cpo)
015100081121     C+ union
015200081121     C+ select dec(ntcnk1), '04', ntctnt, ntcrnt
015300081121     C+ from tfntc00f
015400081121     C+ where ntctnt = '85' and
015500081124     C+ ntcflt<>'A'  and ntcapl = 'P'
015600081124     C+ and dec(ntcnk1) in (select §_cpocpo from
015700081121     C+ QTEMP/iva_cpo)
015800081121     C+ union
015900081121     C+ select dec(ntcnk1), '05', ntctnt, ntcrnt
016000081121     C+ from tfntc00f
016100081121     C+ where ntctnt = '84' and
016200081124     C+ ntcflt<>'A'  and ntcapl = 'P'
016300081124     C+ and dec(ntcnk1) in (select §_cpocpo from
016400081121     C+ QTEMP/iva_cpo)
016500081121     C+ union
016600081121     C+ select dec(ntcnk1), '06', ntctnt, ntcrnt
016700081121     C+ from tfntc00f
016800081121     C+ where ntctnt = '87' and
016900081124     C+ ntcflt<>'A'  and ntcapl = 'P'
017000081124     C+ and dec(ntcnk1) in (select §_cpocpo from
017100081121     C+ QTEMP/iva_cpo)
017200081121     C+ union
017300081121     C+ select dec(ntcnk1), '07', ntctnt, ntcrnt
017400081121     C+ from tfntc00f
017500081121     C+ where ntctnt = '08' and
017600081124     C+ ntcflt<>'A'  and ntcapl = 'P'
017700081124     C+ and dec(ntcnk1) in (select §_cpocpo from
017800081121     C+ QTEMP/iva_cpo)
017900081121     C/END-EXEC
018000081121
018100081121
018200081121     C/EXEC SQL
018300081121     C+ declare C1 cursor for
018400081121     C+ SELECT * FROM QTEMP/iva_cpo  join QTEMP/iva_cponot on
018500081124     C+ §_cpocpo = §_CPNOK1  ORDER BY §_CPOPIV, §_CPNOPR
018600081121     C/END-EXEC
018700081121
018800081124     C/EXEC SQL
018900081124     C+ open C1
019000081124     C/END-EXEC
019100081124     C
019200081124     C/EXEC SQL
019300081124     C+ Fetch C1 into :DS_CPNOT
019400081124     C/END-EXEC
019500081124     C*
019600081121
019700000914     C*
019800000914     C                   dow       sqlcod = *zeros
019900081124      * recupero i dati anagrafici e scrivo il file per ogni P.IVA
020000081124     c                   If        savpiv <> §_cpopiv
020100081124     C                   exsr      EXE_note
020200081124     c                   eval      savpiv = §_cpopiv
020300081124     c                   endif
020400081124     C*
020500000914     C*
020600000914     C/EXEC SQL
020700081124     C+ Fetch C1 into :DS_CPNOT
020800000914     C/END-EXEC
020900000914     C*
021000000914     C                   enddo
021100000914     C*
021200000914     C/EXEC SQL
021300000914     C+ close C1
021400000914     C/END-EXEC
021500000914     C
021600000914     C*
021700081120     C* Chiudo il file (WFPTL10F)
021800081120     C                   close     wfptl10f
021900000920     C*
022000000914     C                   ENDSR
022100000922     C*------------------------------------------------------------------------*
022200081120     C* EXE_NOTE  Routine di extrazione note potenziali
022300000922     C*------------------------------------------------------------------------*
022400081120     C     EXE_NOTE      BEGSR
022500081124     C*
022600081124     c                   clear                   wfptl100
022700081124      * recupero i dati anagrafici del potenziale
022800081124
022900081124     c     §_cpocpo      chain     tncpo01l
023000100112     c                   If        %found(tncpo01l) and cpocmm <> 0
023100081124     c                   eval      wfptip = 'P'
023200081124     c                   z-add     cpocpo        wfpcpo
023300081124     c                   eval      wfpioc = cpopiv
023400081124     c                   eval      wfprag = cporag
023500110606     c                   eval      wfpind = cpovia
023600110606     c                   eval      wfploc = cpocit
023700110606     c                   eval      wfppro = cpoprv
023800110606     c                   eval      wfpcap = cpocap
023900110606     c                   eval      wfpnaz = cponaz
024000110606     c                   eval      wfpabc = cpoftr
024100081124      * cerco l'indirizzo mail
024200081124     c                   movel     wfpcpo        kntnk1
024300081124     c                   eval      knttnt = §_cpnotn
024400081124     c                   eval      wfpeml = §_cpnorn
024500081124     C* Inserisco veirifica x modifica carattere § in @
024600081124     C     '§':'@'       xlate     wfpeml        wfpeml
024700081124     C                   eval      wfptne = §_cpnotn
024800081124     c
024900081124     C* ...x reperire i campi relativi al responsabile SOLO X TIPO NOTA 02 o 05
025000081124     C* ...ovvero se tipo nota e-mail reperito in precedenza è 03 o 06
025100081124     C                   if        wfptne  = '03'
025200081124     C                   eval      KNTTNT = '02'
025300081124     C     KEYNTC        chain     tfntc01l
025400081124     C                   if        %found(tfntc01l) and ntcflt <> 'A'
025500081124     C                   eval      wfprsp = ntcrnt
025600081124     C                   eval      wfptnr = ntctnt
025700081124     C                   endif
025800081124     C                   endif
025900081124     C*
026000081124     C                   if        wfptne  = '06'
026100081124     C                   eval      KNTTNT = '05'
026200081124     C     KEYNTC        chain     tfntc01l
026300081124     C                   if        %found(tfntc01l) and ntcflt <> 'A'
026400081124     C                   eval      wfprsp = ntcrnt
026500081124     C                   eval      wfptnt = ntctnt
026600081124     C                   endif
026700081124     C                   endif
026800081124     C*
026900081124      * p.iva e cpdoce fiscale
027000081124     c                   eval      wfppiva = cpopiv
027100081124     c                   eval      wfpcodf = cpocdf
027200081124
027300081124      * commerciale
027400081124     C                   z-add     cpocmm        wfpcom
027500130829     C* Reperisco il nome ("01") e l'e-mail ("02") del commerciale
027600131003     C                   eval      AZNTCcmm = WFPcom
027700131003     C                   eval      AZNTCtnt = '01'
027800130829     C     k_AZNTC01     chain     AZNTC000
027900130829     C                   if        %found(AZNTC01L)
028000131003     C                   movel     AZNTCrnt      wfpdec
028100130829     C                   endif
028200131003     C                   eval      AZNTCtnt = '02'
028300130829     C     k_AZNTC01     chain     AZNTC000
028400130829     C                   if        %found(AZNTC01L)
028500131003     C                   movel     AZNTCrnt      wfpemc
028600081124     C                   endif
028700081124
028800081124     C* Estrapolo la filiale del commerciale
028900081124     C                   movel     wfpcom        depfil            3 0
029000081124     C     depfil        chain     azorg01l
029100081124     C                   if        %found(azorg01l)
029200081124     C                   z-add     orgfil        wfpfil
029300081124     C                   z-add     orgcar        wfpare
029400081124     C                   movel     orgfl3        wfpdiv
029500081124     C                   movel     orgdes        wfpdes
029600081124     C                   movel     orgind        wfpinf
029700081124     C                   z-add     orgcpf        wfpcpf
029800081124     C                   movel     orgloc        wfplof
029900081124     C                   movel     orgpro        wfpprf
030000081124     C                   movel     orgtel        wfptef
030100081124     C                   movel     orgfax        wfpfaf
030200081124     C                   endif
030300081124
030400081124     c                   write     wfptl100
030500081124
030600081124     C                   endif
030700081124     C                   ENDSR
030800020923     C*--------------------------------------------------------------------------------------------*
030900020923     C* REPERISCE I DATI UTENTE
031000020923     C*--------------------------------------------------------------------------------------------*
031100020923     C     REPDATIUTE    BEGSR
031200020923     C*
031300020923     C* INIZIALIZZA VARIABILI DI WRK
031400020923     C                   CLEAR                   TIBS34DS
031500020923     C                   CLEAR                   AZUTEDS
031600020923     C                   CLEAR                   DDATIUTE
031700020923     C*
031800020923     C     *DTAARA       DEFINE    §azute        azuteds
031900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
032000020923     C                   IN(E)     *DTAARA
032100020923if  1C                   IF        %Error
032200020923     c                   EVAL      I34Tla = 'L'
032300020923     C                   CALL      'TIBS34R'
032400020923     C                   PARM                    Tibs34Ds
032500020923     C                   IN        *DTAARA
032600020923e   1C                   ENDIF
032700020923     C*
032800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
032900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
033000020923     C*
033100020923     C                   ENDSR
033200000000     C*------------------------------------------------------------------------*
033300000000     C* *INZSR - ROUTINE INIZIALE
033400000000     C*------------------------------------------------------------------------*
033500000000     C     *INZSR        BEGSR
033600000000     C*--------------------
033700000000     C* RICEVIMENTO PARAMETRI
033800000000     C*--------------------
033900000000     C     *ENTRY        PLIST
034000000000     C                   PARM                    KPJBA
034100020923     C*
034200020923     C* REPERISCE I DATI DELL'UTENTE
034300020923     C                   EXSR      REPDATIUTE
034400081120     C                   MOVEL(P)  'P'           KNTAPL
034500000922     C*
034600000000     C*--------------------
034700000000     C* CHIAVI DI LETTURA
034800000000     C*--------------------
034900050711     C*
035000050711     C* CHIAVE LETTURA TFNTC01L - COMPLETA
035100050711     C     KEYNTC        KLIST
035200050711     C                   KFLD                    KNTAPL
035300050711     C                   KFLD                    KNTNK1
035400050711     C                   KFLD                    KNTNK2
035500050711     C                   KFLD                    KNTTNT
035600020220     C*
035700130829     C* CHIAVE LETTURA AZNTC01L - COMPLETA
035800130829     C     K_AZNTC01     KLIST
035900131003     C                   KFLD                    AZNTCCMM
036000131003     C                   KFLD                    AZNTCTNT
036100000000     C*
036200000000     C                   ENDSR
