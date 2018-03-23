000100000000     H*------------------------------------------------------------------------*
000200071228     H* CREAZIONE FILE DATI CLIENTI/FORNITORI PER POSTEL
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000914     H*------------------------------------------------------------------------*
000600000925     FAZORG01L  IF   E           K DISK
000700081120     FWFPTL10F  IF A E           K DISK    USROPN
000800020925     FTFNTC01L  IF   E           K DISK
000900081120     FTncpo01L  IF   E           K DISK
001000020220     FTABEL00F  IF   E           K DISK
001100060202     FTNTBE01L  IF   E           K DISK
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*------------------------------------------------------------------------*
001600000000     D* INPUT
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* ARCHITETTURA
002000000000     D*-------------------
002100000000     D KPJBA         E DS
002200020220     D*-------------------
002300060202     D* DS X LETTURA TABELLA "01" => COMMERCIALI
002400020220     D*-------------------
002500020220     D DS01          E DS
002600060202     D*-------------------
002700060202     D* DS X LETTURA TABELLA "HTM" => E-MAIL COMMERCIALI
002800060202     D*-------------------
002900060202     D DHTM          E DS
003000000914     D*------------------------------------------------------------------------*
003100000918     D* VARIABILI DI WRK
003200000914     D*------------------------------------------------------------------------*
003300081120     d command         S             80
003400081120     d piva            s             16
003500081124     d savpiv          s             16
003600081120     d T_nota          s              2
003700081120     d nota_ok         s              1
003800081120     d codcpo          s                   like(cpocpo)
003900081120
004000081120     d KNTapl          s                   like(ntcapl) inz('P')
004100081120     d KNTnk1          s                   like(ntcnk1)
004200081120     d KNTnk2          s                   like(ntcnk2)
004300081120     d KNTtnt          s                   like(ntctnt)
004400081120     D*------------------
004500000921     D* DS "XSRDA8" - CONTROLLA DATA (8)
004600000921     D*------------------
004700000921     D WLBDA8          DS                  INZ
004800000921     D  G08DAT                 1      8  0
004900000921     D  G08INV                 9     16  0
005000000921     D  G08ERR                17     17
005100000921     D  G08TGI                18     22  0
005200081124     D*------------------
005300081124     D* DS DI WRK
005400081124     D*------------------
005500081124     D DS_cpnot        DS
005600081124     D  §_cpopiv                     16
005700081124     D  §_cpocpo                     11  0
005800081124     D  §_cpnok1                     15  0
005900081124     D  §_cpnopr                      2
006000081124     D  §_cpnotn                      2
006100081124     D  §_cpnorn                     60
006200020923     D*-------------------
006300020923     D* DS REPERIMENTO DATI UTENTE
006400020923     D*-------------------
006500020923     D TIBS34DS      E DS                                                       *Profili utente
006600020923     D DDATIUTE      E DS                                                       *Dati utente
006700020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006800000000     C*------------------------------------------------------------------------*
006900000000     C* MAIN LINE
007000000000     C*------------------------------------------------------------------------*
007100071212     C*
007200071212     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007300071212     C
007400071212     C/EXEC SQL
007500071212     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007600071212     C/END-EXEC
007700000000     C*
007800000920     C                   exsr      procedi
007900050404     C*
008000000914     C*
008100000914     C                   SETON                                        LR
008200000922     C*------------------------------------------------------------------------*
008300000922     C* PROCEDI - Routine principale
008400000922     C*------------------------------------------------------------------------*
008500000922     C     PROCEDI       BEGSR
008600000922     C*
008700081120     C* CLRPFM del file (WFPTL10F)
008800030306     C                   Z-ADD     80            LENGH            15 5
008900081120     c                   eval      command = 'CLRPFM FILE(WFPTL10F)'
009000081124     C                   CALL      'QCMDEXC'                            60      *LANCIA CMD
009100081120     C                   PARM                    Command
009200030307     C                   PARM                    LENGH
009300071231     C*
009400081120     C* Apro il file (WFPTL10F)
009500081120     C                   open      wfptl10f
009600000922     C*
009700000922     C* Se riuscito comando ==> procedo
009800081124     C                   if        not *in60
009900081120     C                   exsr      exepotenz
010000081120     c                   endif
010100071231     C*
010200081120     C* Chiudo il file (WFPTL10F)
010300081120     C                   close     wfptl10f
010400000922     C*
010500000922     C                   ENDSR
010600000914     C*------------------------------------------------------------------------*
010700081120     C* EXEPOTENZ - Routine di estrazione potenziali
010800000914     C*------------------------------------------------------------------------*
010900081120     C     EXEPOTENZ     BEGSR
011000000914     C*
011100081121
011200081121     C/EXEC SQL
011300081121     C+ CREATE TABLE QTEMP/IVA_CPO  (
011400081121     C+ §_CPOPIV CHAR (16) NOT NULL WITH DEFAULT,
011500081121     C+ §_CPOCPO DEC (11, 0) NOT NULL WITH DEFAULT)
011600081121     C/END-EXEC
011700081121
011800000914     C/EXEC SQL
011900081121     C+ INSERT INTO QTEMP/IVA_CPO SELECT
012000081121     C+ distinct cpopiv, CPOCPO
012100081121     C+ from tfntc00f joIn tncpo06l
012200081120     C+ on dec(ntcnk1) = cpocpo where
012300081120     C+ ntcapl = 'P' and CPOpiv <> ' '  and
012400081120     C+ ntctnt in ('06', '03', '88', '85',
012500081120     C+ '84', '87', '08') and ntcflt <> 'A'
012600081120     C+ and cpopiv not in (select indiva from cnind00f)
012700000914     C/END-EXEC
012800000914     C
012900081121     C/EXEC SQL
013000081121     C+ CREATE TABLE QTEMP/IVA_CPONOT (
013100081121     C+ §_CPNOK1 DEC (11, 0) NOT NULL WITH DEFAULT,
013200081121     C+ §_CPNOPR CHAR (2) NOT NULL WITH DEFAULT,
013300081121     C+ §_CPNOTN CHAR (2) NOT NULL WITH DEFAULT,
013400081121     C+ §_CPNORN CHAR (60) NOT NULL WITH DEFAULT)
013500081121     C/END-EXEC
013600081121
013700081121
013800081121     C/EXEC SQL
013900081121     C+ INSERT INTO QTEMP/IVA_CPONOT SELECT
014000081121     C+ dec(ntcnk1), '01', ntctnt, ntcrnt
014100081121     C+ from tfntc00f
014200081121     C+ where ntctnt = '06' and
014300081124     C+ ntcflt<>'A' and ntcapl = 'P'
014400081124     C+ and dec(ntcnk1) in (select §_cpocpo from
014500081121     C+ QTEMP/iva_cpo)
014600081121     C+ union
014700081121     C+ select dec(ntcnk1), '02', ntctnt, ntcrnt
014800081121     C+ from tfntc00f
014900081121     C+ where ntctnt = '03' and
015000081124     C+ ntcflt<>'A'  and ntcapl = 'P'
015100081124     C+ and dec(ntcnk1) in (select §_cpocpo from
015200081121     C+ QTEMP/iva_cpo)
015300081121     C+ union
015400081121     C+ select dec(ntcnk1), '03', ntctnt, ntcrnt
015500081121     C+ from tfntc00f
015600081121     C+ where ntctnt = '88' and
015700081124     C+ ntcflt<>'A' and ntcapl = 'P'
015800081124     C+ and dec(ntcnk1) in (select §_cpocpo from
015900081121     C+ QTEMP/iva_cpo)
016000081121     C+ union
016100081121     C+ select dec(ntcnk1), '04', ntctnt, ntcrnt
016200081121     C+ from tfntc00f
016300081121     C+ where ntctnt = '85' and
016400081124     C+ ntcflt<>'A'  and ntcapl = 'P'
016500081124     C+ and dec(ntcnk1) in (select §_cpocpo from
016600081121     C+ QTEMP/iva_cpo)
016700081121     C+ union
016800081121     C+ select dec(ntcnk1), '05', ntctnt, ntcrnt
016900081121     C+ from tfntc00f
017000081121     C+ where ntctnt = '84' and
017100081124     C+ ntcflt<>'A'  and ntcapl = 'P'
017200081124     C+ and dec(ntcnk1) in (select §_cpocpo from
017300081121     C+ QTEMP/iva_cpo)
017400081121     C+ union
017500081121     C+ select dec(ntcnk1), '06', ntctnt, ntcrnt
017600081121     C+ from tfntc00f
017700081121     C+ where ntctnt = '87' and
017800081124     C+ ntcflt<>'A'  and ntcapl = 'P'
017900081124     C+ and dec(ntcnk1) in (select §_cpocpo from
018000081121     C+ QTEMP/iva_cpo)
018100081121     C+ union
018200081121     C+ select dec(ntcnk1), '07', ntctnt, ntcrnt
018300081121     C+ from tfntc00f
018400081121     C+ where ntctnt = '08' and
018500081124     C+ ntcflt<>'A'  and ntcapl = 'P'
018600081124     C+ and dec(ntcnk1) in (select §_cpocpo from
018700081121     C+ QTEMP/iva_cpo)
018800081121     C/END-EXEC
018900081121
019000081121
019100081121     C/EXEC SQL
019200081121     C+ declare C1 cursor for
019300081121     C+ SELECT * FROM QTEMP/iva_cpo  join QTEMP/iva_cponot on
019400081124     C+ §_cpocpo = §_CPNOK1  ORDER BY §_CPOPIV, §_CPNOPR
019500081121     C/END-EXEC
019600081121
019700081124     C/EXEC SQL
019800081124     C+ open C1
019900081124     C/END-EXEC
020000081124     C
020100081124     C/EXEC SQL
020200081124     C+ Fetch C1 into :DS_CPNOT
020300081124     C/END-EXEC
020400081124     C*
020500081121
020600000914     C*
020700000914     C                   dow       sqlcod = *zeros
020800081124      * recupero i dati anagrafici e scrivo il file per ogni P.IVA
020900081124     c                   If        savpiv <> §_cpopiv
021000081124     C                   exsr      EXE_note
021100081124     c                   eval      savpiv = §_cpopiv
021200081124     c                   endif
021300081124     C*
021400000914     C*
021500000914     C/EXEC SQL
021600081124     C+ Fetch C1 into :DS_CPNOT
021700000914     C/END-EXEC
021800000914     C*
021900000914     C                   enddo
022000000914     C*
022100000914     C/EXEC SQL
022200000914     C+ close C1
022300000914     C/END-EXEC
022400000914     C
022500000914     C*
022600081120     C* Chiudo il file (WFPTL10F)
022700081120     C                   close     wfptl10f
022800000920     C*
022900000914     C                   ENDSR
023000000922     C*------------------------------------------------------------------------*
023100081120     C* EXE_NOTE  Routine di extrazione note potenziali
023200000922     C*------------------------------------------------------------------------*
023300081120     C     EXE_NOTE      BEGSR
023400081124     C*
023500081124     c                   clear                   wfptl100
023600081124      * recupero i dati anagrafici del potenziale
023700081124
023800081124     c     §_cpocpo      chain     tncpo01l
023900100112     c                   If        %found(tncpo01l) and cpocmm <> 0
024000081124     c                   eval      wfptip = 'P'
024100081124     c                   z-add     cpocpo        wfpcpo
024200081124     c                   eval      wfpioc = cpopiv
024300081124     c                   eval      wfprag = cporag
024400110606     c                   eval      wfpind = cpovia
024500110606     c                   eval      wfploc = cpocit
024600110606     c                   eval      wfppro = cpoprv
024700110606     c                   eval      wfpcap = cpocap
024800110606     c                   eval      wfpnaz = cponaz
024900110606     c                   eval      wfpabc = cpoftr
025000081124      * cerco l'indirizzo mail
025100081124     c                   movel     wfpcpo        kntnk1
025200081124     c                   eval      knttnt = §_cpnotn
025300081124     c                   eval      wfpeml = §_cpnorn
025400081124     C* Inserisco veirifica x modifica carattere § in @
025500081124     C     '§':'@'       xlate     wfpeml        wfpeml
025600081124     C                   eval      wfptne = §_cpnotn
025700081124     c
025800081124     C* ...x reperire i campi relativi al responsabile SOLO X TIPO NOTA 02 o 05
025900081124     C* ...ovvero se tipo nota e-mail reperito in precedenza è 03 o 06
026000081124     C                   if        wfptne  = '03'
026100081124     C                   eval      KNTTNT = '02'
026200081124     C     KEYNTC        chain     tfntc01l
026300081124     C                   if        %found(tfntc01l) and ntcflt <> 'A'
026400081124     C                   eval      wfprsp = ntcrnt
026500081124     C                   eval      wfptnr = ntctnt
026600081124     C                   endif
026700081124     C                   endif
026800081124     C*
026900081124     C                   if        wfptne  = '06'
027000081124     C                   eval      KNTTNT = '05'
027100081124     C     KEYNTC        chain     tfntc01l
027200081124     C                   if        %found(tfntc01l) and ntcflt <> 'A'
027300081124     C                   eval      wfprsp = ntcrnt
027400081124     C                   eval      wfptnt = ntctnt
027500081124     C                   endif
027600081124     C                   endif
027700081124     C*
027800081124      * p.iva e cpdoce fiscale
027900081124     c                   eval      wfppiva = cpopiv
028000081124     c                   eval      wfpcodf = cpocdf
028100081124
028200081124      * commerciale
028300081124     C                   z-add     cpocmm        wfpcom
028400081124     C* Reperisco la descrizione del commerciale
028500081124     C*                  z-add     1             tblkut
028600081124     C*                  movel     '01'          tblcod
028700081124     C*                  movel(p)  wfpcom        tblkey
028800081124     C*    keytab        chain     tabel00f
028900081124     C*                  if        %found(tabel00f)
029000081124     C*                  movel     tbluni        DS01
029100081124     C*                  movel     §01AGE        wfpdec
029200081124     C*                  endif
029300081124     C* Reperisco l'e-mail del commerciale
029400081124     C                   movel     'HTM'         tbecod
029500081124     C                   movel(p)  wfpcom        tbeke1
029600081124     C                   movel(p)  'CC'          tbeke2
029700081124     C     keytbe        chain     tntbe01l
029800081124     C                   if        %found(tntbe01l)
029900081124     C                   movel     tbeuni        DHTM
030000081124     C                   movel     §HTMNMC       wfpdec
030100081124     C                   movel     §HTMIND       wfpemc
030200081124     C                   endif
030300081124
030400081124     C* Estrapolo la filiale del commerciale
030500081124     C                   movel     wfpcom        depfil            3 0
030600081124     C     depfil        chain     azorg01l
030700081124     C                   if        %found(azorg01l)
030800081124     C                   z-add     orgfil        wfpfil
030900081124     C                   z-add     orgcar        wfpare
031000081124     C                   movel     orgfl3        wfpdiv
031100081124     C                   movel     orgdes        wfpdes
031200081124     C                   movel     orgind        wfpinf
031300081124     C                   z-add     orgcpf        wfpcpf
031400081124     C                   movel     orgloc        wfplof
031500081124     C                   movel     orgpro        wfpprf
031600081124     C                   movel     orgtel        wfptef
031700081124     C                   movel     orgfax        wfpfaf
031800081124     C                   endif
031900081124
032000081124     c                   write     wfptl100
032100081124
032200081124     C                   endif
032300081124     C                   ENDSR
032400020923     C*--------------------------------------------------------------------------------------------*
032500020923     C* REPERISCE I DATI UTENTE
032600020923     C*--------------------------------------------------------------------------------------------*
032700020923     C     REPDATIUTE    BEGSR
032800020923     C*
032900020923     C* INIZIALIZZA VARIABILI DI WRK
033000020923     C                   CLEAR                   TIBS34DS
033100020923     C                   CLEAR                   AZUTEDS
033200020923     C                   CLEAR                   DDATIUTE
033300020923     C*
033400020923     C     *DTAARA       DEFINE    §azute        azuteds
033500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
033600020923     C                   IN(E)     *DTAARA
033700020923if  1C                   IF        %Error
033800020923     c                   EVAL      I34Tla = 'L'
033900020923     C                   CALL      'TIBS34R'
034000020923     C                   PARM                    Tibs34Ds
034100020923     C                   IN        *DTAARA
034200020923e   1C                   ENDIF
034300020923     C*
034400020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
034500020923     C                   MOVEL(P)  RSUT          DSFIRS           20
034600020923     C*
034700020923     C                   ENDSR
034800000000     C*------------------------------------------------------------------------*
034900000000     C* *INZSR - ROUTINE INIZIALE
035000000000     C*------------------------------------------------------------------------*
035100000000     C     *INZSR        BEGSR
035200000000     C*--------------------
035300000000     C* RICEVIMENTO PARAMETRI
035400000000     C*--------------------
035500000000     C     *ENTRY        PLIST
035600000000     C                   PARM                    KPJBA
035700020923     C*
035800020923     C* REPERISCE I DATI DELL'UTENTE
035900020923     C                   EXSR      REPDATIUTE
036000081120     C                   MOVEL(P)  'P'           KNTAPL
036100000922     C*
036200000000     C*--------------------
036300000000     C* CHIAVI DI LETTURA
036400000000     C*--------------------
036500050711     C*
036600050711     C* CHIAVE LETTURA TFNTC01L - COMPLETA
036700050711     C     KEYNTC        KLIST
036800050711     C                   KFLD                    KNTAPL
036900050711     C                   KFLD                    KNTNK1
037000050711     C                   KFLD                    KNTNK2
037100050711     C                   KFLD                    KNTTNT
037200020220     C*
037300020220     C* CHIAVE LETTURA TABEL00F - COMPLETA
037400020220     C     KEYTAB        KLIST
037500020220     C                   KFLD                    TBLKUT
037600020220     C                   KFLD                    TBLCOD
037700020220     C                   KFLD                    TBLKEY
037800060202     C*
037900060202     C* CHIAVE LETTURA TNTBE01L - PARZIALE
038000060202     C     KEYTBE        KLIST
038100060202     C                   KFLD                    TBECOD
038200060202     C                   KFLD                    TBEKE1
038300060203     C                   KFLD                    TBEKE2
038400000000     C*
038500000000     C                   ENDSR
