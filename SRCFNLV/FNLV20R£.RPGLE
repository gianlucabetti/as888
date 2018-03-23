000100041108     H DECEDIT('0,') DATEDIT(*yMd.)
000200941025     H* FNLV20R  *---------------------------------------------------*
000300941025     H*     CALCOLO VOLUME DA FATTURARE                             *
000400921021     H*--------------------------------------------------------------*
000500941031     FAZORG01L  IF   E           K DISK
000600941031     FTABEL00F  IF   E           K DISK
000700941123     FFNARBV0F  O    E           K DISK    USROPN
000800041104     FFNAgv00F  O    E             DISK    USROPN
000900941031     FFNARBV0T  O    E           K DISK    USROPN
001000941031     F                                     RENAME(FNARBV00:FNARBVTT)
001100941031     D*
001200021008     D CMD4            S             48    DIM(1) CTDATA PERRCD(1)
001300041109     D CMDdly          S             48    DIM(1) CTDATA PERRCD(1)
001400941031     D***
001500041217     D WLBDAT          DS                  INZ
001600041217     D  G02DAT                 1      8  0
001700041217     D  G02INV                 9     16  0
001800041217     D  G02ERR                17     17
001900041217     D  G02TGI                18     22  0
002000041217     d
002100941025     D* DS PER FNLV20R - DS DEI CAMPI PER CALCOLO VOLUME DA FATTURARE
002200941025     D DSLV20        E DS                  EXTNAME(FNLV20DS)
002300941031     D* DS TIPI VOLUME
002400941025     D DS7T          E DS
002500941031     D* DS CAUSALI VARIAZIONI
002600941031     D DS7L          E DS
002700941031     D* DS DATI CODICE BOLLA
002800941031     D DS3A          E DS
002900941025     D* DS DATI VOLUME CML
003000070706     D*** DS3I          E DS
003100040123     D og150         E DS
003200970826     D DSLV55        E DS                  EXTNAME(FNLV55DS)
003300941031     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003400941031     D CNCR80        E DS
003500941031     D KPJBA         E DS
003600030603     d og143         e ds
003700021008
003800041108     d  MbrFis         S             10    INZ('B946046')
003900041108     d  wapri          s              1    inz(' ')
004000030603
004100030603     d savlna          s                   like(d20lna)
004200030603     d lnantw          s                   like(§ogntw)
004300040503     d lnpntw          s                   like(§ogntw)
004400921021     C*---------------------------------------------------------------*
004500941031     C* RIEPILOGO INDICATORI
004600941031     C* 01    - PER CML: VOLUME PARZIALE
004700941031     C* 02    - PER CML: VOLUME TOTALE
004800941031     C* 06    - ESEGUO LA TRASMISISONE IN FILIALE
004900941031     C* 26    - ERRORE IN APERTURA FILE FNARBV0T
005000960930     C* 30/34 - DI COMODO
005100941031     C*---------------------------------------------------------------*
005200921021     C     *ENTRY        PLIST
005300040429     c* n.b.: il secondo parametro non è obbligatorio.
005400040429     c*       Se il chiamante lo ha passato con 'N', questo pgm non
005500040429     c*       effettuerà la storicizzazione della variazione
005600941031     C                   PARM                    KPJBA
005700040429     c                   parm                    pwstor            1
005800040510     c
005900941031     C                   MOVEL     KPJBU         DSLV20
006000941231     C                   Z-ADD     1             CODUT
006100000000     C*---------------------------------------------------------------*
006200941025     C* TIPO LANCIO, D20TLA "C" -           CHIUSO CON LR
006300941025     C* TIPO LANCIO, D20TLA "L" - ELABORO E CHIUDO CON LR
006400941025     C* TIPO LANCIO, D20TLA " " - ELABORO E CHIUDO IN RETRN
006500941025    1C     D20TLA        IFNE      'C'
006600941025     C                   CLEAR                   D20ERR
006700041217     c* Calcolo la TIME
006800041217     C                   TIME                    W0140            14 0
006900041217     C* UDATE IN GGMMAAAA
007000041217     C                   MOVE      W0140         WDTGIO            8 0
007100041217     C*
007200041217     C* UDATE IN AAAAMMGG
007300041217     C                   Z-ADD     WDTGIO        G02DAT
007400041217     C                   MOVEL     *BLANK        G02ERR
007500041217     C                   CALL      'XSRDA8'
007600041217     C                   PARM                    WLBDAT
007700041217     C                   MOVEL     G02INV        DATEU             8 0
007800941027     C*
007900941027     C* VEDO QUALE VOLUME HO MODIFICATO
008000941027     C                   SELECT
008100941027     C     D20TVL        WHENEQ    'B'
008200941027     C                   EXSR      GESVLB
008300941025     C*
008400941027     C     D20TVL        WHENEQ    'F'
008500941027     C                   EXSR      GESVLF
008600941027     C*
008700941027     C     D20TVL        WHENEQ    'C'
008800941027     C                   EXSR      GESVLC
008900941027     C                   ENDSL
009000941031    1C                   ENDIF
009100941031     C*
009200941031     C* CHIUSURA PGM
009300941031     C*
009400941031     C                   MOVEL     DSLV20        KPJBU
009500941031     C*
009600941031    1C     D20TLA        IFEQ      ' '
009700941031     C                   RETURN
009800941031     C                   ELSE
009900971125     C                   MOVEL     'C'           D55TLA
010000971125     C                   CALL      'FNLV55R'
010100971125     C                   PARM                    DSLV55
010200941031     C                   SETON                                        LR
010300941031    1C                   ENDIF
010400941027     C*
010500941027     C* MODIFICATO IL VOLUME DA FATTURARE ----------------------------*
010600941027     C     GESVLF        BEGSR
010700030603
010800030603      * Solo il pgm FNLR48R richiama LV20 con flag 'F'
010900030603      * LR48 non permette le variazioni per volumi di tipo "T"
011000030603      * quindi non faccio nessuna modifica x FedEx
011100030603
011200941027     C* IN PARTENZA VARIO SEMPRE ANCHE IL VOLUME BOLLETTATO SE
011300941027     C* IL FLAG VOLUME DA FATTURARE NON E' "Z" O "T"
011400941027     C     D20TBO        IFEQ      'P'
011500941027     C*
011600941027     C     D20FVF        IFNE      'Z'
011700941027     C     D20FVF        ANDNE     'T'
011800941027     C                   MOVEL     D20VLU        D20VLB
011900941031     C                   MOVEL     D20FVL        D20FVB
012000941027     C                   ENDIF
012100941027     C                   ENDIF
012200941027     C*
012300941027     C* IN ARRIVO   VARIO IL VOLUME BOLLETTATO SOLO SE ERA AUTOMATICO
012400941027     C* ED HO RICALCOLATO UN NUOVO VOLUME AUTOMATICO
012500941027     C     D20TBO        IFEQ      'A'
012600941104     C     D20TBO        OREQ      ' '
012700941027     C*
012800941027     C     D20FVL        IFEQ      'K'
012900941027     C     D20FVL        OREQ      'C'
013000941027     C                   MOVEL     D20VLU        D20VLB
013100941031     C                   MOVEL     D20FVL        D20FVB
013200941027     C                   ENDIF
013300941027     C                   ENDIF
013400941027     C*
013500941027     C* IMPOSTO IL VOLUME VARIATO NEL VOLUME D FATTURARE
013600941027     C                   MOVEL     D20VLU        D20VLF
013700941031     C                   MOVEL     D20FVL        D20FVF
013800941027     C                   ENDSR
013900941027     C*
014000941027     C* MODIFICATO IL VOLUME BOLLETTATO ------------------------------*
014100941027     C     GESVLB        BEGSR
014200941027     C*
014300941027     C* SE VLF    E' "T" NON LO VARIO
014400941027     C*
014500941027     C* SE VLC NON C'E' --> VLF = VLB
014600941031    1C     D20FVF        IFNE      'T'
014700941031    2C     D20FVF        IFNE      'Z'
014800941027     C                   Z-ADD     D20VLU        D20VLF
014900941027     C                   MOVEL     D20FVL        D20FVF
015000941025    2C                   ENDIF
015100941027     C*
015200941027     C* SE VLF    E' "Z" MAGGIORE DEL NUOVO BOLLETTATO NON LO VARIO
015300941027     C* SE VLF    E' "Z" MINORE IMPOSTO IL BOLLETTATO IN VLF
015400941031    2C     D20FVF        IFEQ      'Z'
015500941027     C     D20VLF        ANDLT     D20VLU
015600941027     C                   Z-ADD     D20VLU        D20VLF
015700941027     C                   MOVEL     D20FVL        D20FVF
015800941027    2C                   ENDIF
015900941031    2C                   ENDIF
016000941027     C*
016100941027     C                   Z-ADD     D20VLU        D20VLB
016200941027     C                   MOVEL     D20FVL        D20FVB
016300941025     C*
016400941027     C                   ENDSR
016500941027     C*
016600941027     C* MODIFICATO IL VOLUME CML -------------------------------------*
016700941027     C     GESVLC        BEGSR
016800971113     C                   SETOFF                                       26
016900941123     C*
017000941123     C     WUNO          IFEQ      ' '
017100990702     C                   OPEN      FNARBV0F                             98
017200041104     C                   OPEN      FNAgv00F                             98
017300941123     C                   MOVEL     '1'           WUNO              1
017400941123     C                   ENDIF
017500941025     C*
017600941025     C* DETERMINO SE VOLUME PARZIALE O TOTALE
017700941025     C                   EXSR      CTRVLC
017800941025     C*
017900941025     C* DETERMINO CHI HA CML E CHI NO
018000941025     C                   EXSR      CTRCML
018100941025     C**
018200941025     C* T R A S M I S S I O N E
018300941025     C**
018400001011     C** LA ESEGUO COMUNQUE ANCHE SE VOLUME E' UGUALE PERCHE' POTREBBE
018500001011     C*     ESSERE STATA TRASMESSA LA BOLLA CON BLPNCR=99999
018600970818    1C     D20FTR        IFEQ      'S'
018700941025     C                   EXSR      GESTRA
018800970818    1C                   ENDIF
018900001011     C*
019000001011     C*SE IL VOLUME CALCOLATO E' = AL QUELLO DA FATTURARE CHE C'E' GIA'
019100041108     C*  NON FACCIO NULLA
019200001011    0C     WFVF          IFNE      D20FVF
019300001011     C     D20VLU        ORNE      D20VLF
019400941025     C**
019500941025     C* S O S T I T U Z I O N E
019600941031     C**
019700941031     C* LA ESEGUO SOLO SE E' RIUSCITA L'APERTURA DEO FILE DI TRASMISSS
019800941031     C*  ALTRIMENTI NON FACCIO NULLA PERCHE' RITENTO TUTTO UN'ALTRA VOL
019900041108    1C                   IF        d20err=*blanks
020000941025     C*
020100970611     C* SE VOLUME PARZIALE < VLF --> NON FACCIO NIENTE
020200970611     C*  ANCHE SENZA VEDERE SE LNP HA O NON HA CML
020300970611     C*
020400070706    2C     WFVF          IFEQ      'T'
020500070706     C     D20VLU        ORGT      D20VLF
020600941031     C**
020700941031     C* S T O R I C I Z Z A Z I O N E
020800941031     C**
020900041108    3C     WSOST         IFEQ      'S'
021000040510     c* se ricevuto secondo parametro, controllo se storicizzo
021100040510   4ac                   if        %parms < 2 or
021200040510     c                             pwstor<>'N'
021300040429     c
021400941031     C* VEDO SE E' DA STORICIZZARE IL VOLUME DA FATTURARE PRIMA DI SOST
021500970611    4C     WCVBS         IFNE      WCVB
021600040322     c     d20tbo        orne      savtbo
021700941031     C                   MOVEL     '7L'          COD
021800941031     C                   MOVEL(P)  WCVB          KEY
021900941031     C     KTAB          CHAIN     TABEL                              30
022000941031     C  N30              MOVEL     TBLUNI        DS7L
022100941031     C   30              CLEAR                   DS7L
022200941031     C                   MOVEL     WCVB          WCVBS
022300040322     C                   MOVEL     d20tbo        savtbo
022400040322     c
022500040322     c                   clear                   wstor
022600941031     C* DETERMINO QUALE FLAG PRENDERE
022700060130     c
022800060130    6C     SIMFEL        IFGT      0
022900060130     C                   MOVEL     §7LFSA        WSTOR             1
023000060130    6C                   ENDIF
023100941031     C* DETERMINO QUALE FLAG PRENDERE
023200970611    5C     D20TBO        IFEQ      'S'
023300960503     C     SIMFEL        OREQ      0
023400941031     C                   MOVEL     §7LFSS        WSTOR             1
023500970611    5C                   ENDIF
023600970611    4C                   ENDIF
023700941031     C*
023800970611    4C     WSTOR         IFEQ      'S'
023900941031     C                   CLEAR                   FNARBV00
024000941031     C                   Z-ADD     D20AAS        ARBAAS
024100941031     C                   Z-ADD     D20LNP        ARBLNP
024200941031     C                   Z-ADD     D20NRS        ARBNRS
024300941031     C                   Z-ADD     D20NSP        ARBNSP
024400941031     C                   MOVEL     D20CBO        ARBCBO
024500941031     C                   MOVEL     D20FVF        ARBFVC
024600941031     C                   Z-ADD     D20VLF        ARBVLC
024700941031     C                   TIME                    ARBORV
024800041217     C                   MOVEL     dateu         ARBDTV
024900941031     C                   MOVEL     WCVB          ARBCVB
025000941031     C                   WRITE     FNARBV00
025100970611    4C                   ENDIF
025200040429   4aC                   ENDIF
025300941025     C*
025400941025     C* ESEGUO LA SOSTITUZIONE
025500941031     C                   MOVEL     D20VLU        D20VLF
025600941025     C                   MOVEL     WFVF          D20FVF
025700941025    3C                   ENDIF
025800070706    2C                   ENDIF
025900941025    1C                   ENDIF
026000970818    0C                   ENDIF
026100941031     C*
026200041109     C                   ENDSR
026300941021     C*
026400941024     C* OPERAZIONI INIZIALI ------------------------------------------*
026500941024     C     *INZSR        BEGSR
026600941025     C     KTAB          KLIST
026700941025     C                   KFLD                    CODUT             1 0
026800941025     C                   KFLD                    COD               2
026900941025     C                   KFLD                    KEY               8
027000941025     C*
027100941031     C*
027200941031     C     *LIKE         DEFINE    D20LNP        WTFP
027300941031     C     *LIKE         DEFINE    ARBCVB        WCVB
027400941031     C     *LIKE         DEFINE    ARBCVB        WCVBS
027500040322     C     *LIKE         DEFINE    d20tbo        savtbo
027600941024     C                   ENDSR
027700941025     C*
027800941025     C* DETERMINO SE VOLUME TOTALE O PARIZIALE -----------------------*
027900941025     C     CTRVLC        BEGSR
028000941025     C                   SETOFF                                       0102
028100941025     C*
028200941025     C* VOLUME PARZIALE
028300050111     C     D20ncl        IFGT      D20NCR
028400941031     C                   MOVEL     'Z'           WFVF              1
028500941025     C                   SETON                                        01
028600941025     C                   ELSE
028700941025     C* VOLUME TOTALE
028800941025     C                   MOVEL     'T'           WFVF
028900941025     C                   SETON                                        02
029000941025     C                   ENDIF
029100941031     C*
029200941031     C                   MOVEL     WFVF          D20FVL
029300941025     C*
029400941025     C                   ENDSR
029500941025     C*
029600941025     C* DETERMINO CHI HA CML E CHI NO --------------------------------*
029700941025     C     CTRCML        BEGSR
029800941025     C                   CLEAR                   WSOST
029900941025     C                   CLEAR                   WINSE
030000941025     C                   CLEAR                   WINFI
030100941031     C*
030200941031     C* CHAIN DEL TIPO BOLLA
030300941031     C                   MOVEL     '3A'          COD
030400941031     C                   MOVEL(P)  D20CBO        KEY
030500941031     C     KTAB          CHAIN     TABEL                              30
030600941031     C  N30              MOVEL     TBLUNI        DS3A
030700941031     C  N30              MOVEL     §3ATB1        FLGTB1            1
030800941031     C   30              MOVEL     'F'           FLGTB1
030900941025     C*
031000941025     C* CARICO IN TABELLA LE DS DEI TIPI VOLUME CML "T" E "Z"
031100941031    1C     WCART         IFEQ      ' '
031200941025     C                   EXSR      CARTAB
031300941031     C                   MOVEL     '1'           WCART             1
031400941031    1C                   ENDIF
031500941025     C*
031600970826     C* A CAMBIO LINEA DI PARTENZA DETERMINO SE HO O MENO IL CML
031700970826     C     D20LNP        IFNE      SAVLNP
031800941025     C*
031900970826     C* REPERISCO IL TERMINAL PARTENZA :
032000941025     C                   Z-ADD     *ZEROS        PROV              3 0
032100970825     C                   CLEAR                   DSLV55
032200970825     C                   MOVEL     'P'           D55TPT
032300970825     C                   MOVEL     D20LNP        D55LIN
032400970825     C                   MOVEL     D20DTE        D55DRF
032500970825     C                   CALL      'FNLV55R'
032600970825     C                   PARM                    DSLV55
032700960503     C*
032800970825     C     D55ERR        IFEQ      ' '
032900970825     C                   Z-ADD     D55TFP        WTFP
033000970825     C                   ELSE
033100970825     C                   Z-ADD     D20LNP        WTFP
033200970825     C                   ENDIF
033300941025     C*
033400941025     C* CONTROLLO SE IL TERMINAL DI PARTENZA HA IL MACCHINONE CML
033500941025     C     WTFP          CHAIN     AZORG01L                           30
033600941025     C   30              MOVEL     *BLANKS       ORGDF0
033700040123     c                   eval      og150=orgdf0
033800040123     c                   if        §ogspv='S'
033900941025     C                   MOVEL     'S'           WCMLPA
034000941025     C                   ELSE
034100970826     C                   MOVEL     'N'           WCMLPA            1
034200941031    2C                   ENDIF
034300040503      * Cerco il network
034400040503     c                   Clear                   og143
034500040503     c     d20lnp        Chain     Azorg01l
034600040503     c                   If        %Found(Azorg01l)
034700040503     c                   Movel     orgde3        og143
034800040503     c                   EndIf
034900040503     c                   Eval      lnpntw = §ogntw
035000040503     c
035100970826     C                   MOVEL     D20LNP        SAVLNP
035200970826     C     *LIKE         DEFINE    D20LNP        SAVLNP
035300941031    1C                   ENDIF
035400030603
035500030603      * a cambio di linea arrivo cerco il network
035600030603     c                   If        d20lna <> savlna
035700030603     c                   Clear                   og143
035800030603     c     d20lna        Chain     Azorg01l
035900030603     c                   If        %Found(Azorg01l)
036000030603     c                   Movel     orgde3        og143
036100030603     c                   EndIf
036200030603     c                   Eval      lnantw = §ogntw
036300030603     c                   Eval      savlna = d20lna
036400030603     c                   EndIf
036500970826     C**
036600970826     C* CAUSALE VARIAZIONE
036700970826     C**
036800970826     C                   MOVEL     'VV'          WCVB
036900970826     C**
037000970826     C* FLAG SOSTITUZIONE
037100970826     C**
037200030603      * se network della lna è FedEx e volume da variare è "T"
037300030603      * devo sempre sostituire
037400030603
037500030603If  0c                   If        lnantw = 'FED' and wfvf = 'T'
037600030603     c                   Eval      wsost = 'S'
037700030603   x0c                   Else
037800030603
037900970826    1C     FLGTB1        IFEQ      'F'
038000970826    2C     WCMLPA        IFEQ      'S'
038100970826     C   01              MOVEL     Z7TAFA        WSOST             1
038200970826     C   02              MOVEL     T7TAFA        WSOST             1
038300970826     C                   ELSE
038400970826     C   01              MOVEL     Z7TAFN        WSOST             1
038500970826     C   02              MOVEL     T7TAFN        WSOST             1
038600970826    2C                   ENDIF
038700970826     C*
038800970826   X1C                   ELSE
038900970826     C*
039000970826    2C     WCMLPA        IFEQ      'S'
039100970826     C   01              MOVEL     Z7TAAA        WSOST             1
039200970826     C   02              MOVEL     T7TAAA        WSOST             1
039300970826     C                   ELSE
039400970826     C   01              MOVEL     Z7TAAN        WSOST             1
039500970826     C   02              MOVEL     T7TAAN        WSOST             1
039600970826    2C                   ENDIF
039700970826    1C                   ENDIF
039800970826     C**
039900970826    1C     WSOST         IFNE      ' '
040000970826     C                   MOVEL     '7T'          COD
040100970826     C                   MOVEL(P)  D20FVF        KEY
040200970826     C     KTAB          CHAIN     TABEL                              30
040300970826    2C     *IN30         IFEQ      *OFF
040400970826     C                   MOVEL     TBLUNI        DS7T
040500970826    3C     WFVF          IFEQ      'Z'
040600070615     c* Se SOZ è vuoto, prendo quelli dei franchi o assegnati
040700070615     c                   if        §7tsoz=' '
040800070615     c
040900070615     c                   if        flgtb1='F'
041000070615     C                   MOVEL     §7TSOZf       WSOST
041100070615     c                   else
041200070615     C                   MOVEL     §7TSOZa       WSOST
041300070615     c                   endif
041400070615     c*
041500070615     c                   else
041600970826     C                   MOVEL     §7TSOZ        WSOST
041700070615     c                   endif
041800070615     c
041900970826   X3C                   ELSE
042000070615     c
042100070615     c* Se SOT è vuoto, prendo quelli dei franchi o assegnati
042200070615     c                   if        §7tsot=' '
042300070615     c                   if        flgtb1='F'
042400070615     C                   MOVEL     §7TSOTf       WSOST
042500070615     c                   else
042600070615     C                   MOVEL     §7TSOTa       WSOST
042700070615     c                   endif
042800070615     c
042900070615     c                   else
043000970826     C                   MOVEL     §7TSOT        WSOST
043100070615     c                   endif
043200970826    3C                   ENDIF
043300970826     C                   CLEAR                   DS7T
043400970826    2C                   ENDIF
043500970826    1C                   ENDIF
043600030603
043700030603If  0c                   EndIf
043800040503c
043900040503     c* lnp euroexpress --> sostituisco sempre, anche REALE
044000040503     c                   if        lnpntw='EEX'
044100040503     c                   eval      wsost='S'
044200040503     c                   endif
044300970826     C**
044400970826     C* FALG DI TRASMISSIONE
044500970826     C**
044600970826     C*  PARTENZA
044700941025     C**
044800970902    0C     D20FTR        IFEQ      'S'
044900970902     C**
045000941031    1C     D20TBO        IFEQ      'P'
045100941025     C*
045200941031    2C     FLGTB1        IFEQ      'F'
045300941025     C* TRASMISSIONE IN SEDE
045400941031     C   01              MOVEL     Z7TPSF        WINSE             1
045500941031     C   02              MOVEL     T7TPSF        WINSE             1
045600041108     C* TRASMISSIONE IN parternza: non c'e' sono in partenza
045700941025     C*
045800941031   X2C                   ELSE
045900941025     C*
046000941025     C* TRASMISSIONE IN SEDE
046100941031     C   01              MOVEL     Z7TPSA        WINSE             1
046200941031     C   02              MOVEL     T7TPSA        WINSE             1
046300041108     C* TRASMISSIONE IN partenza: non c'e' sono in partenza
046400941031    2C                   ENDIF
046500941031    1C                   ENDIF
046600941025     C**
046700970826     C* ARRIVO
046800941025     C**
046900941025    1C     D20TBO        IFEQ      'A'
047000941025     C* FILIALE DI TRASMISSIONE
047100970826    2C     FLGTB1        IFEQ      'F'
047200970826     C* TRASMISSIONE IN SEDE
047300970826     C   01              MOVEL     Z7TASF        WINSE             1
047400970826     C   02              MOVEL     T7TASF        WINSE             1
047500041108     C* TRASMISSIONE IN partenza
047600970826     C   01              MOVEL     Z7TAPF        WINFI             1
047700970826     C   02              MOVEL     T7TAPF        WINFI             1
047800970826   X2C                   ELSE
047900941025     C* TRASMISSIONE IN SEDE
048000941031     C   01              MOVEL     Z7TASA        WINSE             1
048100941031     C   02              MOVEL     T7TASA        WINSE             1
048200041108     C* TRASMISSIONE IN partenza
048300941031     C   01              MOVEL     Z7TAPA        WINFI             1
048400941031     C   02              MOVEL     T7TAPA        WINFI             1
048500941025    2C                   ENDIF
048600041108     c* Trasmettere alla partenza
048700041108     c                   if        winfi='S'
048800041108     c                   eval      winfi='P'
048900041108     c                   endif
049000941025    1C                   ENDIF
049100941025     C**
049200941025     C* TRANSITO
049300941025     C**
049400941025    1C     D20TBO        IFEQ      'T'
049500941025     C*
049600941025    2C     FLGTB1        IFEQ      'F'
049700941025     C* TRASMISSIONE IN SEDE
049800941031     C   01              MOVEL     Z7TTSF        WINSE             1
049900941031     C   02              MOVEL     T7TTSF        WINSE             1
050000041108     C* TRASMISSIONE IN partenza
050100041108     C   01              MOVEL     Z7TTPF        WINFi             1
050200041108     C   02              MOVEL     T7TTPF        WINFi             1
050300941025     C*
050400941025   X2C                   ELSE
050500941025     C*
050600941025     C* TRASMISSIONE IN SEDE
050700941031     C   01              MOVEL     Z7TTSA        WINSE             1
050800941031     C   02              MOVEL     T7TTSA        WINSE             1
050900041108     C* TRASMISSIONE IN partenza
051000041108     C   01              MOVEL     Z7TTPA        WINFi             1
051100041108     C   02              MOVEL     T7TTPA        WINFi             1
051200941025     C*
051300941031    2C                   ENDIF
051400041108     c* Trasmettere alla partenza
051500041108     c                   if        winfi='S'
051600041108     c                   eval      winfi='P'
051700041108     c                   endif
051800941031    1C                   ENDIF
051900970902    0C                   ENDIF
052000941025     C*
052100941025     C                   ENDSR
052200941025     C*
052300941025     C* CARICO LE 2 DS DEI TIPI VOLUME ------------------------------*
052400941025     C     CARTAB        BEGSR
052500941031     C*
052600941031     C                   Z-ADD     1             CODUT             1 0
052700941031     C                   CALL      'X§PARUT'
052800941031     C                   PARM                    UT§DSE
052900941031     C                   MOVEL     REC80         CNCR80
053000941031     C                   MOVEL     RAGUT         RSUT             20
053100040505     C* SE KNSIF=*BLANKS IMPOSTO *LIBL
053200040505     C                   IF        KNSIF=*BLANKS
053300040505     C                   EVAL      KNSIF='*LIBL'
053400040505     C                   ENDIF
053500941031     C*
053600021008     C                   Z-ADD     48            LUNG
053700941025     C*
053800941025     C* CARICO FLAGS TIPO VOLUME 'T'
053900941025     C                   MOVEL     '7T'          COD
054000941025     C                   MOVEL(P)  'T'           KEY
054100941025     C     KTAB          CHAIN     TABEL00F                           32
054200941025     C*
054300941025     C  N32              MOVEL     TBLUNI        DS7T
054400941025     C   32              CLEAR                   DS7T
054500970826     C* SOSTITUZIONE VLS A VOL FRANCHI   CON LNP CON   CML
054600970826     C                   MOVEL     §7TAFA        T7TAFA            1
054700970826     C* SOSTITUZIONE VLS A VOL ASSEGNATI CON LNP CON   CML
054800970826     C                   MOVEL     §7TAAA        T7TAAA            1
054900970826     C* SOSTITUZIONE VLS A VOL FRANCHI   CON LNP SENZA CML
055000970826     C                   MOVEL     §7TAFN        T7TAFN            1
055100970826     C* SOSTITUZIONE VLS A VOL ASSEGNATI CON LNP SENZA CML
055200970826     C                   MOVEL     §7TAAN        T7TAAN            1
055300941025     C*
055400941025     C* ARR.:INVIO PARTEN FRANCHI
055500941025     C                   MOVEL     §7TAPF        T7TAPF            1
055600941025     C* ARR.:INVIO PARTEN ASSEGNATI
055700941025     C                   MOVEL     §7TAPA        T7TAPA            1
055800941025     C* ARR.:INVIO SEDE FRANCHI
055900941025     C                   MOVEL     §7TASF        T7TASF            1
056000941025     C* ARR.:INVIO SEDE ASSEGNATI
056100941025     C                   MOVEL     §7TASA        T7TASA            1
056200941025     C*
056300941025     C*
056400941025     C* PAR.:INVIO SEDE FRANCHI
056500941025     C                   MOVEL     §7TPSF        T7TPSF            1
056600941025     C* PAR.:INVIO SEDE ASSEGNATI
056700941025     C                   MOVEL     §7TPSA        T7TPSA            1
056800941025     C*
056900941025     C* TRAN:INVIO PARTEN FRANCHI
057000941025     C                   MOVEL     §7TTPF        T7TTPF            1
057100941025     C* TRAN:INVIO PARTEN ASSEGNATI
057200941025     C                   MOVEL     §7TTPA        T7TTPA            1
057300941025     C* TRAN:INVIO SEDE FRANCHI
057400941025     C                   MOVEL     §7TTSF        T7TTSF            1
057500941025     C* TRAN:INVIO SEDE ASSEGNATI
057600941025     C                   MOVEL     §7TTSA        T7TTSA            1
057700941025     C*
057800941025     C* CARICO FLAGS TIPO VOLUME 'Z'
057900941025     C                   MOVEL     '7T'          COD
058000941025     C                   MOVEL     *BLANKS       KEY
058100941025     C                   MOVEL     'Z'           KEY
058200941025     C     KTAB          CHAIN     TABEL00F                           32
058300941025     C*
058400941025     C  N32              MOVEL     TBLUNI        DS7T
058500941025     C   32              CLEAR                   DS7T
058600970826     C* SOSTITUZIONE VLS A VOL FRANCHI   CON LNP CON   CML
058700970826     C                   MOVEL     §7TAFA        Z7TAFA            1
058800970826     C* SOSTITUZIONE VLS A VOL ASSEGNATI CON LNP CON   CML
058900970826     C                   MOVEL     §7TAAA        Z7TAAA            1
059000970826     C* SOSTITUZIONE VLS A VOL FRANCHI   CON LNP SENZA CML
059100970826     C                   MOVEL     §7TAFN        Z7TAFN            1
059200970826     C* SOSTITUZIONE VLS A VOL ASSEGNATI CON LNP SENZA CML
059300970826     C                   MOVEL     §7TAAN        Z7TAAN            1
059400941025     C*
059500941025     C* ARR.:INVIO PARTEN FRANCHI
059600941025     C                   MOVEL     §7TAPF        Z7TAPF            1
059700941025     C* ARR.:INVIO PARTEN ASSEGNATI
059800941025     C                   MOVEL     §7TAPA        Z7TAPA            1
059900941025     C* ARR.:INVIO SEDE FRANCHI
060000941025     C                   MOVEL     §7TASF        Z7TASF            1
060100941025     C* ARR.:INVIO SEDE ASSEGNATI
060200941025     C                   MOVEL     §7TASA        Z7TASA            1
060300941025     C*
060400941025     C*
060500941025     C* PAR.:INVIO SEDE FRANCHI
060600941025     C                   MOVEL     §7TPSF        Z7TPSF            1
060700941025     C* PAR.:INVIO SEDE ASSEGNATI
060800941025     C                   MOVEL     §7TPSA        Z7TPSA            1
060900941025     C*
061000941025     C* TRAN:INVIO PARTEN FRANCHI
061100941025     C                   MOVEL     §7TTPF        Z7TTPF            1
061200941025     C* TRAN:INVIO PARTEN ASSEGNATI
061300941025     C                   MOVEL     §7TTPA        Z7TTPA            1
061400941025     C* TRAN:INVIO SEDE FRANCHI
061500941025     C                   MOVEL     §7TTSF        Z7TTSF            1
061600941025     C* TRAN:INVIO SEDE ASSEGNATI
061700941025     C                   MOVEL     §7TTSA        Z7TTSA            1
061800941025     C**
061900941025     C**
062000970826     C* SE SONO IN SEDE QUESTE ULTIME 3 COSE NON
062100941025     C*  LE FACCIO PERCHE' NON C'E' TRASMISSIONE
062200941025     C**
062300070706     C***  D20TBO        IFNE      ' '
062400941025     C**
062500941025     C* LETTURA PERCENTUALE SCOSTAMENTO
062600941025     C**
062700070706     C***                MOVEL     '3I'          COD
062800070706     C***                MOVEL(P)  '1'           KEY
062900070706     C***  KTAB          CHAIN     TABEL00F                           32
063000070706IF  1C***  *IN32         IFEQ      '0'
063100070706     C***                MOVEL     TBLUNI        DS3I
063200070706E   1C***                ENDIF
063300070706     C***                ENDIF
063400941025     C                   ENDSR
063500941025     C*
063600941025     C* GESTIONE TRASMISSIONE VOLUME CML -----------------------------*
063700941025     C     GESTRA        BEGSR
063800941025     C*
063900070706     C***                SETOFF                                       06
064000941025     C**
064100041108     C* aggiorno in partenza
064200941025     C**
064300041108IF  1C     WINFI         IFne      ' '
064400941025     C*
064500941025     C* SE VOLUME TOTALE DA CML TRASMETTO SEMPRE
064600941025     C*
064700070706IF  2C***  WFVF          IFEQ      'T'
064800070706     C***                SETON                                        06
064900941025     C*
065000941025     C* VOLUME PARZIALE DA CML
065100070706X   2C***                ELSE
065200941025     C*
065300941025     C* VOLUME NUOVO DEVE ESSERE MAGGIORE DEL VECCHIO
065400070706IF  3C***  D20VLU        IFGT      D20VLF
065500941025     C*
065600941025     C* PULISCO CAMPI DI COMODO
065700070706     C***                Z-ADD     *ZEROS        COMVOL
065800070706     C***                Z-ADD     *ZEROS        VOLCON
065900941025     C*
066000941025     C* TRASFORMO IN VALORE ASSOLUTO LA PERCENTUALE
066100070706     C***  D20VLF        MULT      §3IPSC        COMVOL            5 3
066200070706     C***  COMVOL        DIV       100           COMVOL            5 3
066300941025     C*
066400941025     C* VOLUME VECCHIO + LA % DI SCOSTAMENTO
066500070706     C***  D20VLF        ADD       COMVOL        VOLCON            5 3
066600941025     C*
066700941025     C* NUOVO VOLUME DEVE ESSERE MAGGIORE DEL VECCHIO + % SCOSTAMENTO
066800070706IF  4C***  D20VLU        IFGT      VOLCON
066900070706     C***                SETON                                        06
067000070706E   4C***                ENDIF
067100070706E   3C***                ENDIF
067200070706E   2C***                ENDIF
067300941025     C*
067400041108     C* 06 ON - aggiorno in partenza
067500070706IF  2C***  *IN06         IFEQ      *ON
067600041108     C                   movel     winfi         agvtbo
067700041108     C                   Z-ADD     D20AAS        agvAAS
067800041108     C                   Z-ADD     D20LNP        agvLNP
067900041108     C                   Z-ADD     D20NRS        agvNRS
068000041108     C                   Z-ADD     D20NSP        agvNSP
068100041108     C                   MOVEL     D20CBO        agvCBO
068200041108     C                   MOVEL     WFVF          agvFvC
068300041108     C                   Z-ADD     D20vLU        agvvlc
068400041108     C                   Z-ADD     D20nCr        agvNCr
068500041108     C                   TIME                    agvORV
068600041217     C                   MOVEL     dateu         agvDTV
068700041108     C                   MOVEL     WCVB          agvCVB
068800041108     c                   write     fnagv000
068900041108     c                   feod      fnagv00f
069000941025     C*
069100070706E   2C***                ENDIF
069200941025E   1C                   ENDIF
069300941031     C**
069400941025     C* INVIO A  S E D E
069500941031     C**
069600941025     C* FRANCHI O ASSEGNATI
069700941031IF  1C     WINSE         IFEQ      'S'
069800941025     C*
069900041108     C* Solo la prima volta apro il membro
070000041108    2c                   if        wapri=*blanks
070100941031     C                   EXSR      CAEXT
070200041108     c* 26 off - no errori
070300041108     c                   if        *in26=*off
070400041108     c                   eval      wapri='S'
070500041108     c                   else
070600041108     c                   eval      d20err='1'
070700041108     c                   endif
070800041108    2c                   endif
070900941025     C*
071000941025     C* SCRIVO SOLO SE HO APERTO IL MEMBRO
071100041108IF  2C                   IF        d20err=*blanks
071200941031     C                   Z-ADD     D20AAS        ARBAAS
071300941031     C                   Z-ADD     D20LNP        ARBLNP
071400941031     C                   Z-ADD     D20NRS        ARBNRS
071500941031     C                   Z-ADD     D20NSP        ARBNSP
071600941031     C                   MOVEL     D20CBO        ARBCBO
071700941031     C                   MOVEL     WFVF          ARBFVC
071800941031     C                   Z-ADD     D20VLU        ARBVLC
071900941031     C                   Z-ADD     D20NCR        ARBNCR
072000941031     C                   TIME                    ARBORV
072100041217     C                   MOVEL     dateu         ARBDTV
072200941031     C                   MOVEL     WCVB          ARBCVB
072300941031     C                   WRITE     FNARBVTT
072400080408     c                   feod      fnarbv0t
072500041108    2c                   endif
072600041108    1c                   endif
072700941025     C*
072800941025     C                   ENDSR
072900941031     C*---------------------------------------------------------------*
073000941031     C*                    CAEXT                                      *
073100941031     C* ROUTINE DI APERTURA MEMBRO TRASMISSIONE                       *
073200941031     C*---------------------------------------------------------------*
073300941031     C     CAEXT         BEGSR
073400041109     c                   clear                   writenta
073500041108     c
073600041108     c* Verifico esistenza membro
073700040510     c                   EXSR      ADDmembro
073800041109     c     riopen        tag
073900941031     C* OVRDBF
074000041108     C                   MOVEL     *BLANKS       COMMAN           80
074100941031     C                   MOVEA     CMD4(1)       COMMAN
074200040510     C                   CALL      'QCMDEXC'
074300941031     C                   PARM                    COMMAN
074400041108     C                   PARM                    LUNG             15 5
074500941031     C*
074600041108     C                   OPEN      FNARBV0T                             26
074700041109     c                   if        writenta<2  and *in26
074800041109     c* se open non riuacita --> ritento con un dly
074900041109     C                   MOVEL     *BLANKS       COMMAN           80
075000041109     C                   MOVEA     CMDdly(1)     COMMAN
075100041109     C                   CALL      'QCMDEXC'
075200041109     C                   PARM                    COMMAN
075300041109     C                   PARM                    LUNG             15 5
075400041109     c
075500041109     c                   add       1             writenta          2 0
075600041109     c                   goto      riopen
075700041109     c                   endif
075800041109     c
075900941031     C*
076000941031     C                   ENDSR
076100040510     C*
076200040510     c* Vedo se manca il membro lo aggiungo  ------------------------*
076300040510     c     ADDMEMBRO     begsr
076400040510     C                   MOVEL     *BLANKS       FISICO           10
076500040510     C                   MOVEL     'FNARBV0T'    FISICO
076600040510     C                   MOVEL     *BLANKS       LOGICO           10
076700040510     C                   MOVEL     *BLANKS       LOGMBR           10
076800040510     C                   MOVEL     *BLANKS       FLG               1
076900040510     C*
077000040510     C                   CALL      'TRUL50C'
077100040510     C                   PARM                    FISICO
077200040510     C                   PARM                    MbrFis
077300040510     C                   PARM                    KNSIF
077400040510     C                   PARM                    LOGICO
077500040510     C                   PARM                    LOGMBR
077600040510     C                   PARM                    FLG
077700040510     c                   endsr
077800941031**         CMD4
077900041108OVRDBF FILE(FNARBV0T)               MBR(B946046)
078000041109**         CMDdly
078100041109DLYJOB (30)
