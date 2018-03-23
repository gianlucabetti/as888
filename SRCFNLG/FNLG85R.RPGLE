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
001500060329     Fcnaco00f  IF   E           K DISK
001600941010      *--------------------------------------------------------------*
001700941011     D FL£7            S              3    DIM(29)
001800020503     D £1              S              3  0 DIM(30)
001900941011     D £7              S              3  0 DIM(29)
002000050217     D dd              S              2  0
002100941011     D ERR             S             70    DIM(1) CTDATA PERRCD(1)
002200941010      *--------------------------------------------------------------*
002300900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002400900418     D CNCR80        E DS
002500060809     D fnlg12ds      E DS
002600020503     d trul06ds      e ds
002700020503     d  lin                    1     90  0 dim(30)
002800900418     D KPJBA         E DS
002900941011     D FNLG82        E DS                  EXTNAME(FNLG82DS)
003000950102     D FNLG30        E DS                  EXTNAME(FNLG30DS)
003100060412     D ds2a          E DS
003200941011     D*
003300060809     D savkpjbu        S                   like(kpjbu)
003400020730     D §FLE            S              3  0
003500920113     D                 DS
003600920113     D  GCPMGC                 1      4  0
003700920113     D  MESE                   1      2  0
003800920113     D  GIOR                   3      4  0
003900911017     C*---------------------------------------------------------------*
004000941011     C                   MOVEL     '1'           WTPVID            1
004100060801     C                   MOVE      'S'           vmotiv
004200941011     C*
004300911010     C     *INKC         DOWEQ     '0'
004400941011     C*
004500941011     C     WTPVID        CASEQ     '1'           GESD01
004600941011     C     WTPVID        CASEQ     '2'           GESD02
004700941011     C     WTPVID        CASEQ     '3'           GESD03
004800941011     C                   END
004900941011     C*
005000941011     C                   END
005100941011     C*
005200941011     C                   SETON                                        LR
005300941011     C*--------------------------------------------------------------*
005400941011     C*  GESTIONE DELLA PRIMA VIDEATA
005500941011     C*--------------------------------------------------------------*
005600941011     C     GESD01        BEGSR
005700941011     C*
005800941011     C                   Z-ADD     0             VFGC
005900941011     C                   Z-ADD     0             VAGC
006000911126     C                   Z-ADD     0             VNGC
006100941010     C                   EXFMT     LG85D01
006200950131     C*
006300950131     C                   MOVEL     VAGC          WAA2              2 0
006400950131     C     WAA2          IFEQ      0
006500950131     C     VAGC          IFGT      60
006600950131     C                   ADD       1900          VAGC
006700950131     C                   ELSE
006800950131     C                   ADD       2000          VAGC
006900950131     C                   END
007000950131     C                   END
007100941011     C*
007200941011     C* Fine Elaborazione
007300941011     C     *INKC         IFEQ      '1'
007400941011     C                   GOTO      FINVD1
007500941011     C                   END
007600941011     C* Interrogazione giacenze
007700941011     C     *INKG         IFEQ      '1'
007800950102     C                   CLEAR                   FNLG30
007900950102     C                   MOVEL     'P'           D30POA
008000950102     C                   Z-ADD     VAGC          D30AGC
008100950102     C                   Z-ADD     VFGC          D30FGC
008200950102     C                   Z-ADD     VNGC          D30NGC
008300950102     C                   MOVE      *ZEROS        D30FRG
008400950102     C                   MOVEL     FNLG30        KPJBU
008500950102     C                   CALL      'FNLG30C'
008600950102     C                   PARM                    KPJBA
008700941011     C                   GOTO      FINVD1
008800920826     C                   END
008900911126     C                   Z-ADD     0             GCPFRG
009000060811     c                   if        vngc <> 0
009100050216     C     KGCA          CHAIN     tigcp02L                           50
009200020821     c     gcplnp        lookup    £7                                     49
009300941011     C     *IN50         IFEQ      '1'
009400941011     C     GCPFAS        ORNE      40
009500050217     C     GCPdlm        ORNE      0
009600020821     C     *in49         OReq      *off
009700911126     C                   SETON                                        50
009800941011     C                   ELSE
009900060329      *verifica se cliente con chiusura automatica imposta flag x bypass video
010000060329     c                   clear                   d82nodsp
010100060412      *verifico se il motivo di apertura è a rischio per il progressivo 0
010200060412      * in questo caso la gestione è manuale
010300060412     c                   clear                   motr              1
010400060412     c                   move      '2A'          cod
010500060412     c                   movel(p)  gcpcmc        key
010600060412     c     ktab          chain     tabel00f
010700060412     c                   if        %found(tabel00f)
010800060412     c                   movel     tbluni        ds2a
010900060412     c                   movel     §2avst        motr
011000060412     c                   endif
011100060329     c     kaco          chain     cnaco00f
011200060329     c                   if        %found(cnaco00f)
011300060412     c                   if        acory1 = 0 and gcpchiaut = *blank
011400060412     c                             and motr = *blank
011500060329     c                   move      'S'           d82nodsp
011600060329     c                   endif
011700060329     c                   endif
011800941221     C* FORZO IL PGM STAMPA SEGNACOLLI IN KPJBA
011900941011     C                   MOVE      VAGC          D82AGC
012000941011     C                   MOVE      VNGC          D82NGC
012100941011     C                   MOVE      VFGC          D82FGC
012200060329     C                   MOVE      Vmotiv        D82motr
012300941011     C                   MOVEL     FNLG82        KPJBU
012400941011     C                   CALL      'FNLG15R'
012500911126     C                   PARM                    KPJBA
012600920206     C                   END
012700060811     c                   endif
012800060811
012900060811     C* Chiuse in arrivo
013000060811     C     *INKH         IFEQ      '1'
013100060811     C                   write     LG85D01
013200060811     C                   EXSR      CARICA
013300060811     C                   MOVEL     '2'           WTPVID
013400060811     C                   GOTO      FINVD1
013500060811     C                   END
013600060811     C*
013700941011     C*
013800941011     C     FINVD1        ENDSR
013900941011     C*--------------------------------------------------------------*
014000941011     C*  GESTIONE DELLA SECONDA VIDEATA
014100941011     C*--------------------------------------------------------------*
014200941011     C     GESD02        BEGSR
014300941011     C*
014400941011     C                   MOVEL     'N'           WSELEZ            1
014500060329     c     riemetti      tag
014600941011     C                   WRITE     LG85Z02
014700941011     C                   EXFMT     LG85C02
014800941011     C* Fine Elaborazione
014900941011     C     *INKC         IFEQ      '1'
015000941011     C                   GOTO      FINVD2
015100941011     C                   END
015200941011     C* Ritorno
015300941011     C     *INKL         IFEQ      '1'
015400941011     C                   MOVEL     '1'           WTPVID
015500941011     C                   GOTO      FINVD2
015600941011     C                   END
015700941011     C*  Cambio Filiale
015800941011     C     *INKS         IFEQ      '1'
015900941011     C                   MOVEL     '3'           WTPVID
016000941011     C                   SETOFF                                       2840
016100941011     C                   GOTO      FINVD2
016200941011     C                   END
016300911010     C*
016400060329     C*  imposta opzione 1 su tutte le righe
016500060329     C     *INKa         IFEQ      '1'
016600060329     c                   do        *hival        knrr              4 0
016700060329     c     knrr          chain     lg85s02                            66
016800060329     c   66              leave
016900060329     C                   MOVEL     '1'           vsel
017000060329     c                   seton                                        67
017100060329     C                   update    lg85s02
017200060329     c                   enddo
017300060329     C                   GOTO      riemetti
017400060329     C                   END
017500060329     C*
017600060329     C*  imposta opzione 1 su tutte le righe Automatiche
017700060329     C     *INKg         IFEQ      '1'
017800060329     c                   do        *hival        knrr
017900060329     c     knrr          chain     lg85s02                            66
018000060329     c   66              leave
018100060329     c                   if        vaut = 'A'
018200060329     C                   MOVEL     '1'           vsel
018300060329     c                   else
018400060329     c                   move      *blank        vsel
018500060329     c                   endif
018600060329     c                   seton                                        67
018700060329     C                   update    lg85s02
018800060329     c                   enddo
018900060329     C                   GOTO      riemetti
019000060329     C                   END
019100060329     C*
019200060329     C*  imposta opzione 1 su tutte le righe Manuali
019300060329     C     *INKi         IFEQ      '1'
019400060329     c                   do        *hival        knrr
019500060329     c     knrr          chain     lg85s02                            66
019600060329     c   66              leave
019700060329     c                   if        vaut <> 'A'
019800060329     C                   MOVEL     '1'           vsel
019900060329     c                   else
020000060329     c                   move      *blank        vsel
020100060329     c                   endif
020200060329     c                   seton                                        67
020300060329     C                   update    lg85s02
020400060329     c                   enddo
020500060329     C                   GOTO      riemetti
020600060329     C                   END
020700060329     C*
020800941011     C     *IN60         IFEQ      '0'
020900941011     C                   READC     LG85S02                                30
021000911107     C     *IN30         DOWEQ     '0'
021100060411     C     VSEL          IFEQ      '1'
021200060411     C                   MOVEL     'S'           WSELEZ
021300060411     C                   MOVE      VAGC          D82AGC
021400060411     C                   MOVE      VNGC          D82NGC
021500060411     C                   MOVE      VFGC          D82FGC
021600060411     C                   MOVE      Vmotiv        D82motr
021700060329      * se automatica imposto flag per pgm chiamati bypass videate
021800060411     c                   clear                   d82nodsp
021900060411     c                   if        vaut <> *blank
022000060411     c                   move      'S'           d82nodsp
022100060411     c                   endif
022200060411     C                   MOVEL     FNLG82        KPJBU
022300060411     C                   CALL      'FNLG15R'
022400060411     C                   PARM                    KPJBA
022500060411     C                   END
022600060411     C                   READC     LG85S02                                30
022700911107     C                   END
022800941011     C                   END
022900941011     C*
023000941011     C     WSELEZ        IFEQ      'S'
023100941011     C                   EXSR      CARICA
023200941011     C                   END
023300941011     C*
023400941011     C     FINVD2        ENDSR
023500941011     C*--------------------------------------------------------------*
023600941011     C*  GESTIONE DELLA TERZA VIDEATA
023700941011     C*--------------------------------------------------------------*
023800941011     C     GESD03        BEGSR
023900941011     C*
024000941011     C                   EXFMT     LG85D03
024100941011     C                   SETOFF                                       2840
024200941011     C* Ritorno
024300941011     C     *INKL         IFEQ      '1'
024400941011     C                   MOVEL     '2'           WTPVID
024500941011     C                   GOTO      FINVD3
024600941011     C                   END
024700941011     C*
024800941011     C     §FLE          IFNE      0
024900941011     C     §FLE          LOOKUP    £1                                     32
025000941011     C                   END
025100941011     C*
025200941011     C     *IN32         IFEQ      '0'
025300941011     C                   SETON                                        2840
025400941011     C                   MOVEL     ERR(1)        $MSG
025500941011     C                   ELSE
025600941011     C                   EXSR      CAR£7
025700941011     C                   EXSR      CARICA
025800941011     C                   MOVEL     '2'           WTPVID
025900941011     C                   END
026000941011     C*
026100941011     C     FINVD3        ENDSR
026200941011     C*----------------------------------------------------------*
026300941011     C* Caricamento tabella £7
026400941011     C*----------------------------------------------------------*
026500941011     C     CAR£7         BEGSR
026600941011     C* Caricamento £7
026700941011     C                   MOVEA     *ZEROS        FL£7
026800941011     C                   MOVEL     *BLANKS       KEY
026900941011     C                   MOVEL     §FLE          KEY
027000941011     C                   MOVEL     '£7'          COD
027100941011     C     KTAB          CHAIN     TABEL                              31
027200941011     C     *IN31         IFEQ      '0'
027300941011     C     TBLFLG        ANDEQ     ' '
027400941011     C                   MOVEA     TBLUNI        FL£7
027500941011     C                   ELSE
027600941011     C                   MOVE      §FLE          FL£7(1)
027700941011     C                   END
027800941011     C*
027900941011     C                   Z-ADD     0             £7
028000941011     C                   Z-ADD     0             X£7               2 0
028100941011     C                   DO        29            X                 3 0
028200941011     C     FL£7(X)       IFNE      *BLANKS
028300941011     C                   ADD       1             X£7               2 0
028400941011     C                   MOVE      FL£7(X)       £7(X£7)
028500941011     C                   END
028600941011     C                   END
028700941011     C*
028800941011     C                   ENDSR
028900911017     C*---------------*
029000941010     C*  OPERAZIONI INIZIALI
029100911017     C*---------------*
029200941010     C     *INZSR        BEGSR
029300941010     C*
029400900418     C     *ENTRY        PLIST
029500900418     C                   PARM                    KPJBA
029600941229     C                   MOVEL     KPJBU         FNLG82
029700900418     C                   Z-ADD     1             CODUT
029800900418     C                   CALL      'X§PARUT'
029900900418     C                   PARM                    UTEDSE
030000941011     C                   MOVEL     'P'           D82GES
030100941011     C*
030200900418     C                   MOVEL     RAGUT         RSUT             20
030300900418     C                   MOVEL     RAGUT         RSUT2            25
030400900502     C                   MOVEL     RAGUT         PARDUT           30
030500911028     C                   MOVEL     REC80         CNCR80
030600020730     C     SimTpP        COMP      '2'                                    01
030700020812     C                   Z-ADD     SIMPOU        §FLE
030800900419     C*                                                    *
030900900418     C*  KLIST
031000911010     C     KTAB          KLIST
031100911010     C                   KFLD                    CODUT
031200911010     C                   KFLD                    COD               2
031300911010     C                   KFLD                    KEY               8
031400911010     C*
031500060329     C     Kaco          KLIST
031600060329     C                   KFLD                    CODUT
031700060329     C                   KFLD                    kkcc              4 0
031800060329     C                   KFLD                    gcpscm
031900060329     C                   z-add     151           kkcc
032000060329     C*
032100911106     C     KGCA          KLIST
032200911106     C                   KFLD                    VAGC
032300911106     C                   KFLD                    VFGC
032400911106     C                   KFLD                    VNGC
032500990628     C                   KFLD                    FRG               2 0
032600911107     C                   Z-ADD     0             FRG
032700050216     C     KGCp6         KLIST
032800050216     C                   KFLD                    kfil              3 0
032900050216     C                   KFLD                    fas               3 0
033000050216     c                   z-add     40            fas
033100941011     C* Caricamento £1
033200020503     c                   clear                   trul06ds
033300020503     c                   eval      d06cod = '£1'
033400020503     c                   movel     §fle          d06key
033500020503     c                   movel     trul06ds      kpjbu
033600020503     c                   call      'TRUL06R'
033700020503     c                   parm                    kpjba
033800020503     c                   movel     kpjbu         trul06ds
033900020503     c                   movea     lin           £1
034000941011     C*  Caricamento tabella £7
034100941011     C                   EXSR      CAR£7
034200900418     C                   ENDSR
034300941011     C*---------------------------------------------------------------*
034400911017     C* CARICAMENTO SUBFILE
034500941011     C*---------------------------------------------------------------*
034600911010     C     CARICA        BEGSR
034700911107     C*
034800050216     C                   EXSR      puliz
034900050217     c                   do        29            dd
035000050217     c                   if        £7(dd) = 0
035100050217     c                   iter
035200050217     c                   end
035300050217     c                   eval      kfil = £7(dd)
035400050216     C     kgcp6         SETLL     tigcp06L                           20
035500050216     C     kgcp6         READe     tigcp06L                             1620
035600941011     C*
035700911010     C     *IN20         DOWEQ     '0'
035800941011     C* Considero le giacenze con fase 40
035900941011     C     *IN16         IFEQ      '0'
036000941011     C* Controllo che sia in  schiera
036100050217     C     GCPdlm        andeq     0
036200050217     C                   EXSR      WTR
036300911010     C                   END
036400050216     C     kgcp6         READe     tigcp06L                             1620
036500911010     C                   END
036600050217     C                   ENDdo
036700941011     c*
036800911108     C     NRR           COMP      0                                      60
036900911108     C   60              ADD       1             NRR
037000941011     C   60              WRITE     LG85S02
037100941011     C*
037200911010     C                   ENDSR
037300911017     C*-----------------------------*
037400911017     C* SCRITTURA RECORD NEL SUBFILE
037500911017     C*-----------------------------*
037600911010     C     WTR           BEGSR
037700911106     C                   MOVEL     *BLANKS       VSEL
037800911106     C                   Z-ADD     GCPFGC        VFGC
037900911106     C                   Z-ADD     GCPAGC        VAGC
038000911106     C                   Z-ADD     GCPNGC        VNGC
038100920113     C                   MOVE      MESE          VMGC
038200920113     C                   MOVEL     GIOR          VMGC
038300911107     C                   Z-ADD     GCPAAS        VAAS
038400911107     C                   Z-ADD     GCPNRS        VNRS
038500911107     C                   Z-ADD     GCPNSP        VNSP
038600911107     C                   MOVEL     GCPCFG        VCFG
038700060329      *verifico se per il cliente giacenza la gestione chiusure è automatica
038800060329     c
038900060329     c                   move      *blank        vaut
039000060412      *verifico se il motivo di apertura è a rischio per il progressivo 0
039100060412      * in questo caso la gestione è manuale
039200060412     c                   clear                   motr
039300060412     c                   move      '2A'          cod
039400060412     c                   movel(p)  gcpcmc        key
039500060412     c     ktab          chain     tabel00f
039600060412     c                   if        %found(tabel00f)
039700060412     c                   movel     tbluni        ds2a
039800060412     c                   movel     §2avst        motr
039900060412     c                   endif
040000060809      * richiama pgm per verifica incongruenze se ne trova disabilita automatica
040100060809     c                   movel     kpjbu         savkpjbu
040200060809     c                   clear                   fnlg12ds
040300060809     c                   z-add     gcpagc        i12agc
040400060809     c                   z-add     gcpngc        i12ngc
040500060809     c                   z-add     gcpfgc        i12fgc
040600060809     c                   move      'S'           i12forza
040700060809     c                   move      motr          i12motr
040800060809     c                   movel(p)  fnlg12ds      kpjbu
040900060809     c                   call      'FNLG12R'
041000060809     c                   parm                    kpjba
041100060809     c                   movel     kpjbu         fnlg12ds
041200060809     c                   movel     savkpjbu      kpjbu
041300060329     c     kaco          chain     cnaco00f
041400060329     c                   if        %found(cnaco00f)
041500060811     c                   if        acory1 = 0 and gcpchiaut <> 'N'
041600060809     c                             and motr = *blank and o12ok = *blank
041700060329     c                   move      'A'           vaut
041800060329     c                   endif
041900060329     c                   endif
042000911107     C* DECODIFICO CODICE FINE GIACENZA
042100911108     C     GCPCFG        IFNE      *BLANKS
042200911107     C                   MOVEL     *BLANKS       KEY
042300911107     C                   MOVEL     GCPCFG        KEY
042400911107     C                   MOVEL     '2C'          COD
042500911107     C     KTAB          CHAIN     TABEL                              15
042600911107     C  N15              MOVEL     TBLUNI        DESCFG
042700911107     C   15              MOVEL     *ALL'?'       DESCFG
042800911108     C                   ELSE
042900911108     C                   MOVEL     *BLANKS       DESCFG
043000911108     C                   END
043100911011     C                   ADD       1             NRR               4 0
043200941011     C                   WRITE     LG85S02
043300900509     C                   ENDSR
043400911017     C*----------------*
043500911010     C* PULIZIA SUBFILE
043600911017     C*----------------*
043700911010     C     PULIZ         BEGSR
043800911010     C                   SETON                                        70
043900941011     C                   WRITE     LG85C02
044000911010     C                   Z-ADD     0             NRR
044100911010     C                   SETOFF                                       70
044200911017     C                   MOVEL     *BLANKS       VSEL
044300911017     C                   MOVE      *ZEROS        VFGC
044400911017     C                   MOVE      *ZEROS        VAGC
044500911017     C                   MOVE      *ZEROS        VMGC
044600911017     C                   MOVE      *ZEROS        VNGC
044700911017     C                   MOVE      *ZEROS        VAAS
044800911017     C                   MOVE      *ZEROS        VNRS
044900911017     C                   MOVE      *ZEROS        VNSP
045000911106     C                   MOVE      *BLANKS       VCFG
045100911106     C                   MOVE      *BLANKS       DESCFG
045200911017     C                   ENDSR
045300961114**
045400961114Punto operativo errato
