000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200911107     H* FLFG85R *----------------------------------------------------*
000300900418     H*                                                              *
000400911107     H*             - CONFERMA CHIUSURA GIACENZE -                   *
000500900418     H*  19 IND. ERRORE GENERICO VIDEO                               *
000600900418     H*  21 IND. COMODO                                              *
000700900418     H*                                                              *
000800900418     H*--------------------------------------------------------------*
000900941010     FFNLG85D   CF   E             WORKSTN
001000941010     F                                     SFILE(LG85S02:NRR)
001100050216     Ftigcp06l  IF   E           K DISK
001200050216     Ftigcp02l  IF   E           K DISK
001300050216     F                                     RENAME(tigcp000:tigcp002)
001400911010     FTABEL00F  IF   E           K DISK
001500941010      *--------------------------------------------------------------*
001600941011     D FL£7            S              3    DIM(29)
001700020503     D £1              S              3  0 DIM(30)
001800941011     D £7              S              3  0 DIM(29)
001900050217     D dd              S              2  0
002000941011     D ERR             S             70    DIM(1) CTDATA PERRCD(1)
002100941010      *--------------------------------------------------------------*
002200900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002300900418     D CNCR80        E DS
002400020503     d trul06ds      e ds
002500020503     d  lin                    1     90  0 dim(30)
002600900418     D KPJBA         E DS
002700941011     D FNLG82        E DS                  EXTNAME(FNLG82DS)
002800950102     D FNLG30        E DS                  EXTNAME(FNLG30DS)
002900941011     D*
003000020730     D §FLE            S              3  0
003100920113     D                 DS
003200920113     D  GCPMGC                 1      4  0
003300920113     D  MESE                   1      2  0
003400920113     D  GIOR                   3      4  0
003500911017     C*---------------------------------------------------------------*
003600941011     C                   MOVEL     '1'           WTPVID            1
003700941011     C*
003800911010     C     *INKC         DOWEQ     '0'
003900941011     C*
004000941011     C     WTPVID        CASEQ     '1'           GESD01
004100941011     C     WTPVID        CASEQ     '2'           GESD02
004200941011     C     WTPVID        CASEQ     '3'           GESD03
004300941011     C                   END
004400941011     C*
004500941011     C                   END
004600941011     C*
004700941011     C                   SETON                                        LR
004800941011     C*--------------------------------------------------------------*
004900941011     C*  GESTIONE DELLA PRIMA VIDEATA
005000941011     C*--------------------------------------------------------------*
005100941011     C     GESD01        BEGSR
005200941011     C*
005300941011     C                   Z-ADD     0             VFGC
005400941011     C                   Z-ADD     0             VAGC
005500911126     C                   Z-ADD     0             VNGC
005600941010     C                   EXFMT     LG85D01
005700950131     C*
005800950131     C                   MOVEL     VAGC          WAA2              2 0
005900950131     C     WAA2          IFEQ      0
006000950131     C     VAGC          IFGT      60
006100950131     C                   ADD       1900          VAGC
006200950131     C                   ELSE
006300950131     C                   ADD       2000          VAGC
006400950131     C                   END
006500950131     C                   END
006600941011     C*
006700941011     C* Fine Elaborazione
006800941011     C     *INKC         IFEQ      '1'
006900941011     C                   GOTO      FINVD1
007000941011     C                   END
007100941011     C* Interrogazione giacenze
007200941011     C     *INKG         IFEQ      '1'
007300950102     C                   CLEAR                   FNLG30
007400950102     C                   MOVEL     'P'           D30POA
007500950102     C                   Z-ADD     VAGC          D30AGC
007600950102     C                   Z-ADD     VFGC          D30FGC
007700950102     C                   Z-ADD     VNGC          D30NGC
007800950102     C                   MOVE      *ZEROS        D30FRG
007900950102     C                   MOVEL     FNLG30        KPJBU
008000950102     C                   CALL      'FNLG30C'
008100950102     C                   PARM                    KPJBA
008200941011     C                   GOTO      FINVD1
008300920826     C                   END
008400941011     C* Chiuse in arrivo
008500941011     C     *INKH         IFEQ      '1'
008600050216     C                   EXSR      CARICA
008700941011     C                   MOVEL     '2'           WTPVID
008800941011     C                   GOTO      FINVD1
008900941011     C                   END
009000941011     C*
009100911126     C                   Z-ADD     0             GCPFRG
009200050216     C     KGCA          CHAIN     tigcp02L                           50
009300020821     c     gcplnp        lookup    £7                                     49
009400941011     C     *IN50         IFEQ      '1'
009500941011     C     GCPFAS        ORNE      40
009600050217     C     GCPdlm        ORNE      0
009700020821     C     *in49         OReq      *off
009800911126     C                   SETON                                        50
009900941011     C                   ELSE
010000941221     C* FORZO IL PGM STAMPA SEGNACOLLI IN KPJBA
010100941011     C                   MOVE      VAGC          D82AGC
010200941011     C                   MOVE      VNGC          D82NGC
010300941011     C                   MOVE      VFGC          D82FGC
010400941011     C                   MOVEL     FNLG82        KPJBU
010500941011     C                   CALL      'FNLG15R'
010600911126     C                   PARM                    KPJBA
010700920206     C                   END
010800941011     C*
010900941011     C     FINVD1        ENDSR
011000941011     C*--------------------------------------------------------------*
011100941011     C*  GESTIONE DELLA SECONDA VIDEATA
011200941011     C*--------------------------------------------------------------*
011300941011     C     GESD02        BEGSR
011400941011     C*
011500941011     C                   MOVEL     'N'           WSELEZ            1
011600941011     C                   WRITE     LG85Z02
011700941011     C                   EXFMT     LG85C02
011800941011     C* Fine Elaborazione
011900941011     C     *INKC         IFEQ      '1'
012000941011     C                   GOTO      FINVD2
012100941011     C                   END
012200941011     C* Ritorno
012300941011     C     *INKL         IFEQ      '1'
012400941011     C                   MOVEL     '1'           WTPVID
012500941011     C                   GOTO      FINVD2
012600941011     C                   END
012700941011     C*  Cambio Filiale
012800941011     C     *INKS         IFEQ      '1'
012900941011     C                   MOVEL     '3'           WTPVID
013000941011     C                   SETOFF                                       2840
013100941011     C                   GOTO      FINVD2
013200941011     C                   END
013300911010     C*
013400941011     C     *IN60         IFEQ      '0'
013500941011     C                   READC     LG85S02                                30
013600911107     C     *IN30         DOWEQ     '0'
013700941011     C     VSEL          IFEQ      '1'
013800941011     C                   MOVEL     'S'           WSELEZ
013900941011     C                   MOVE      VAGC          D82AGC
014000941011     C                   MOVE      VNGC          D82NGC
014100941011     C                   MOVE      VFGC          D82FGC
014200941011     C                   MOVEL     FNLG82        KPJBU
014300941011     C                   CALL      'FNLG15R'
014400911010     C                   PARM                    KPJBA
014500911010     C                   END
014600941011     C                   READC     LG85S02                                30
014700911107     C                   END
014800941011     C                   END
014900941011     C*
015000941011     C     WSELEZ        IFEQ      'S'
015100941011     C                   EXSR      CARICA
015200941011     C                   END
015300941011     C*
015400941011     C     FINVD2        ENDSR
015500941011     C*--------------------------------------------------------------*
015600941011     C*  GESTIONE DELLA TERZA VIDEATA
015700941011     C*--------------------------------------------------------------*
015800941011     C     GESD03        BEGSR
015900941011     C*
016000941011     C                   EXFMT     LG85D03
016100941011     C                   SETOFF                                       2840
016200941011     C* Ritorno
016300941011     C     *INKL         IFEQ      '1'
016400941011     C                   MOVEL     '2'           WTPVID
016500941011     C                   GOTO      FINVD3
016600941011     C                   END
016700941011     C*
016800941011     C     §FLE          IFNE      0
016900941011     C     §FLE          LOOKUP    £1                                     32
017000941011     C                   END
017100941011     C*
017200941011     C     *IN32         IFEQ      '0'
017300941011     C                   SETON                                        2840
017400941011     C                   MOVEL     ERR(1)        $MSG
017500941011     C                   ELSE
017600941011     C                   EXSR      CAR£7
017700941011     C                   EXSR      CARICA
017800941011     C                   MOVEL     '2'           WTPVID
017900941011     C                   END
018000941011     C*
018100941011     C     FINVD3        ENDSR
018200941011     C*----------------------------------------------------------*
018300941011     C* Caricamento tabella £7
018400941011     C*----------------------------------------------------------*
018500941011     C     CAR£7         BEGSR
018600941011     C* Caricamento £7
018700941011     C                   MOVEA     *ZEROS        FL£7
018800941011     C                   MOVEL     *BLANKS       KEY
018900941011     C                   MOVEL     §FLE          KEY
019000941011     C                   MOVEL     '£7'          COD
019100941011     C     KTAB          CHAIN     TABEL                              31
019200941011     C     *IN31         IFEQ      '0'
019300941011     C     TBLFLG        ANDEQ     ' '
019400941011     C                   MOVEA     TBLUNI        FL£7
019500941011     C                   ELSE
019600941011     C                   MOVE      §FLE          FL£7(1)
019700941011     C                   END
019800941011     C*
019900941011     C                   Z-ADD     0             £7
020000941011     C                   Z-ADD     0             X£7               2 0
020100941011     C                   DO        29            X                 3 0
020200941011     C     FL£7(X)       IFNE      *BLANKS
020300941011     C                   ADD       1             X£7               2 0
020400941011     C                   MOVE      FL£7(X)       £7(X£7)
020500941011     C                   END
020600941011     C                   END
020700941011     C*
020800941011     C                   ENDSR
020900911017     C*---------------*
021000941010     C*  OPERAZIONI INIZIALI
021100911017     C*---------------*
021200941010     C     *INZSR        BEGSR
021300941010     C*
021400900418     C     *ENTRY        PLIST
021500900418     C                   PARM                    KPJBA
021600941229     C                   MOVEL     KPJBU         FNLG82
021700900418     C                   Z-ADD     1             CODUT
021800900418     C                   CALL      'X§PARUT'
021900900418     C                   PARM                    UTEDSE
022000941011     C                   MOVEL     'P'           D82GES
022100941011     C*
022200900418     C                   MOVEL     RAGUT         RSUT             20
022300900418     C                   MOVEL     RAGUT         RSUT2            25
022400900502     C                   MOVEL     RAGUT         PARDUT           30
022500911028     C                   MOVEL     REC80         CNCR80
022600020730     C     SimTpP        COMP      '2'                                    01
022700020812     C                   Z-ADD     SIMPOU        §FLE
022800900419     C*                                                    *
022900900418     C*  KLIST
023000911010     C     KTAB          KLIST
023100911010     C                   KFLD                    CODUT
023200911010     C                   KFLD                    COD               2
023300911010     C                   KFLD                    KEY               8
023400911010     C*
023500911106     C     KGCA          KLIST
023600911106     C                   KFLD                    VAGC
023700911106     C                   KFLD                    VFGC
023800911106     C                   KFLD                    VNGC
023900990628     C                   KFLD                    FRG               2 0
024000911107     C                   Z-ADD     0             FRG
024100050216     C     KGCp6         KLIST
024200050216     C                   KFLD                    kfil              3 0
024300050216     C                   KFLD                    fas               3 0
024400050216     c                   z-add     40            fas
024500941011     C* Caricamento £1
024600020503     c                   clear                   trul06ds
024700020503     c                   eval      d06cod = '£1'
024800020503     c                   movel     §fle          d06key
024900020503     c                   movel     trul06ds      kpjbu
025000020503     c                   call      'TRUL06R'
025100020503     c                   parm                    kpjba
025200020503     c                   movel     kpjbu         trul06ds
025300020503     c                   movea     lin           £1
025400941011     C*  Caricamento tabella £7
025500941011     C                   EXSR      CAR£7
025600900418     C                   ENDSR
025700941011     C*---------------------------------------------------------------*
025800911017     C* CARICAMENTO SUBFILE
025900941011     C*---------------------------------------------------------------*
026000911010     C     CARICA        BEGSR
026100911107     C*
026200050216     C                   EXSR      puliz
026300050217     c                   do        29            dd
026400050217     c                   if        £7(dd) = 0
026500050217     c                   iter
026600050217     c                   end
026700050217     c                   eval      kfil = £7(dd)
026800050216     C     kgcp6         SETLL     tigcp06L                           20
026900050216     C     kgcp6         READe     tigcp06L                             1620
027000941011     C*
027100911010     C     *IN20         DOWEQ     '0'
027200941011     C* Considero le giacenze con fase 40
027300941011     C     *IN16         IFEQ      '0'
027400941011     C* Controllo che sia in  schiera
027500050217     C     GCPdlm        andeq     0
027600050217     C                   EXSR      WTR
027700911010     C                   END
027800050216     C     kgcp6         READe     tigcp06L                             1620
027900911010     C                   END
028000050217     C                   ENDdo
028100941011     c*
028200911108     C     NRR           COMP      0                                      60
028300911108     C   60              ADD       1             NRR
028400941011     C   60              WRITE     LG85S02
028500941011     C*
028600911010     C                   ENDSR
028700911017     C*-----------------------------*
028800911017     C* SCRITTURA RECORD NEL SUBFILE
028900911017     C*-----------------------------*
029000911010     C     WTR           BEGSR
029100911106     C                   MOVEL     *BLANKS       VSEL
029200911106     C                   Z-ADD     GCPFGC        VFGC
029300911106     C                   Z-ADD     GCPAGC        VAGC
029400911106     C                   Z-ADD     GCPNGC        VNGC
029500920113     C                   MOVE      MESE          VMGC
029600920113     C                   MOVEL     GIOR          VMGC
029700911107     C                   Z-ADD     GCPAAS        VAAS
029800911107     C                   Z-ADD     GCPNRS        VNRS
029900911107     C                   Z-ADD     GCPNSP        VNSP
030000911107     C                   MOVEL     GCPCFG        VCFG
030100911107     C* DECODIFICO CODICE FINE GIACENZA
030200911108     C     GCPCFG        IFNE      *BLANKS
030300911107     C                   MOVEL     *BLANKS       KEY
030400911107     C                   MOVEL     GCPCFG        KEY
030500911107     C                   MOVEL     '2C'          COD
030600911107     C     KTAB          CHAIN     TABEL                              15
030700911107     C  N15              MOVEL     TBLUNI        DESCFG
030800911107     C   15              MOVEL     *ALL'?'       DESCFG
030900911108     C                   ELSE
031000911108     C                   MOVEL     *BLANKS       DESCFG
031100911108     C                   END
031200911011     C                   ADD       1             NRR               4 0
031300941011     C                   WRITE     LG85S02
031400900509     C                   ENDSR
031500911017     C*----------------*
031600911010     C* PULIZIA SUBFILE
031700911017     C*----------------*
031800911010     C     PULIZ         BEGSR
031900911010     C                   SETON                                        70
032000941011     C                   WRITE     LG85C02
032100911010     C                   Z-ADD     0             NRR
032200911010     C                   SETOFF                                       70
032300911017     C                   MOVEL     *BLANKS       VSEL
032400911017     C                   MOVE      *ZEROS        VFGC
032500911017     C                   MOVE      *ZEROS        VAGC
032600911017     C                   MOVE      *ZEROS        VMGC
032700911017     C                   MOVE      *ZEROS        VNGC
032800911017     C                   MOVE      *ZEROS        VAAS
032900911017     C                   MOVE      *ZEROS        VNRS
033000911017     C                   MOVE      *ZEROS        VNSP
033100911106     C                   MOVE      *BLANKS       VCFG
033200911106     C                   MOVE      *BLANKS       DESCFG
033300911017     C                   ENDSR
033400961114**
033500961114Punto operativo errato
