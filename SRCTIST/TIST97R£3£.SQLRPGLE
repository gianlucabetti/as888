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
024400081124      * cerco l'indirizzo mail
024500081124     c                   movel     wfpcpo        kntnk1
024600081124     c                   eval      knttnt = §_cpnotn
024700081124     c                   eval      wfpeml = §_cpnorn
024800081124     C* Inserisco veirifica x modifica carattere § in @
024900081124     C     '§':'@'       xlate     wfpeml        wfpeml
025000081124     C                   eval      wfptne = §_cpnotn
025100081124     c
025200081124     C* ...x reperire i campi relativi al responsabile SOLO X TIPO NOTA 02 o 05
025300081124     C* ...ovvero se tipo nota e-mail reperito in precedenza è 03 o 06
025400081124     C                   if        wfptne  = '03'
025500081124     C                   eval      KNTTNT = '02'
025600081124     C     KEYNTC        chain     tfntc01l
025700081124     C                   if        %found(tfntc01l) and ntcflt <> 'A'
025800081124     C                   eval      wfprsp = ntcrnt
025900081124     C                   eval      wfptnr = ntctnt
026000081124     C                   endif
026100081124     C                   endif
026200081124     C*
026300081124     C                   if        wfptne  = '06'
026400081124     C                   eval      KNTTNT = '05'
026500081124     C     KEYNTC        chain     tfntc01l
026600081124     C                   if        %found(tfntc01l) and ntcflt <> 'A'
026700081124     C                   eval      wfprsp = ntcrnt
026800081124     C                   eval      wfptnt = ntctnt
026900081124     C                   endif
027000081124     C                   endif
027100081124     C*
027200081124      * p.iva e cpdoce fiscale
027300081124     c                   eval      wfppiva = cpopiv
027400081124     c                   eval      wfpcodf = cpocdf
027500081124
027600081124      * commerciale
027700081124     C                   z-add     cpocmm        wfpcom
027800081124     C* Reperisco la descrizione del commerciale
027900081124     C*                  z-add     1             tblkut
028000081124     C*                  movel     '01'          tblcod
028100081124     C*                  movel(p)  wfpcom        tblkey
028200081124     C*    keytab        chain     tabel00f
028300081124     C*                  if        %found(tabel00f)
028400081124     C*                  movel     tbluni        DS01
028500081124     C*                  movel     §01AGE        wfpdec
028600081124     C*                  endif
028700081124     C* Reperisco l'e-mail del commerciale
028800081124     C                   movel     'HTM'         tbecod
028900081124     C                   movel(p)  wfpcom        tbeke1
029000081124     C                   movel(p)  'CC'          tbeke2
029100081124     C     keytbe        chain     tntbe01l
029200081124     C                   if        %found(tntbe01l)
029300081124     C                   movel     tbeuni        DHTM
029400081124     C                   movel     §HTMNMC       wfpdec
029500081124     C                   movel     §HTMIND       wfpemc
029600081124     C                   endif
029700081124
029800081124     C* Estrapolo la filiale del commerciale
029900081124     C                   movel     wfpcom        depfil            3 0
030000081124     C     depfil        chain     azorg01l
030100081124     C                   if        %found(azorg01l)
030200081124     C                   z-add     orgfil        wfpfil
030300081124     C                   z-add     orgcar        wfpare
030400081124     C                   movel     orgfl3        wfpdiv
030500081124     C                   movel     orgdes        wfpdes
030600081124     C                   movel     orgind        wfpinf
030700081124     C                   z-add     orgcpf        wfpcpf
030800081124     C                   movel     orgloc        wfplof
030900081124     C                   movel     orgpro        wfpprf
031000081124     C                   movel     orgtel        wfptef
031100081124     C                   movel     orgfax        wfpfaf
031200081124     C                   endif
031300081124
031400081124     c                   write     wfptl100
031500081124
031600081124     C                   endif
031700081124     C                   ENDSR
031800020923     C*--------------------------------------------------------------------------------------------*
031900020923     C* REPERISCE I DATI UTENTE
032000020923     C*--------------------------------------------------------------------------------------------*
032100020923     C     REPDATIUTE    BEGSR
032200020923     C*
032300020923     C* INIZIALIZZA VARIABILI DI WRK
032400020923     C                   CLEAR                   TIBS34DS
032500020923     C                   CLEAR                   AZUTEDS
032600020923     C                   CLEAR                   DDATIUTE
032700020923     C*
032800020923     C     *DTAARA       DEFINE    §azute        azuteds
032900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
033000020923     C                   IN(E)     *DTAARA
033100020923if  1C                   IF        %Error
033200020923     c                   EVAL      I34Tla = 'L'
033300020923     C                   CALL      'TIBS34R'
033400020923     C                   PARM                    Tibs34Ds
033500020923     C                   IN        *DTAARA
033600020923e   1C                   ENDIF
033700020923     C*
033800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
033900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
034000020923     C*
034100020923     C                   ENDSR
034200000000     C*------------------------------------------------------------------------*
034300000000     C* *INZSR - ROUTINE INIZIALE
034400000000     C*------------------------------------------------------------------------*
034500000000     C     *INZSR        BEGSR
034600000000     C*--------------------
034700000000     C* RICEVIMENTO PARAMETRI
034800000000     C*--------------------
034900000000     C     *ENTRY        PLIST
035000000000     C                   PARM                    KPJBA
035100020923     C*
035200020923     C* REPERISCE I DATI DELL'UTENTE
035300020923     C                   EXSR      REPDATIUTE
035400081120     C                   MOVEL(P)  'P'           KNTAPL
035500000922     C*
035600000000     C*--------------------
035700000000     C* CHIAVI DI LETTURA
035800000000     C*--------------------
035900050711     C*
036000050711     C* CHIAVE LETTURA TFNTC01L - COMPLETA
036100050711     C     KEYNTC        KLIST
036200050711     C                   KFLD                    KNTAPL
036300050711     C                   KFLD                    KNTNK1
036400050711     C                   KFLD                    KNTNK2
036500050711     C                   KFLD                    KNTTNT
036600020220     C*
036700020220     C* CHIAVE LETTURA TABEL00F - COMPLETA
036800020220     C     KEYTAB        KLIST
036900020220     C                   KFLD                    TBLKUT
037000020220     C                   KFLD                    TBLCOD
037100020220     C                   KFLD                    TBLKEY
037200060202     C*
037300060202     C* CHIAVE LETTURA TNTBE01L - PARZIALE
037400060202     C     KEYTBE        KLIST
037500060202     C                   KFLD                    TBECOD
037600060202     C                   KFLD                    TBEKE1
037700060203     C                   KFLD                    TBEKE2
037800000000     C*
037900000000     C                   ENDSR
