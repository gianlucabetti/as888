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
003000941025     D DS3I          E DS
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
020400941031    2C     WFVF          IFEQ      'T'
020500941031     C     D20VLU        ORGT      D20VLF
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
022700060130    5C**   D20TBO        IFEQ      'P'
022800060130     C**   SIMFEL        ANDGT     0
022900060130     C**                 MOVEL     §7LFSP        WSTOR             1
023000060130    5C**                 ENDIF
023100941031     C* DETERMINO QUALE FLAG PRENDERE
023200060130    5C**   D20TBO        IFEQ      'A'
023300060130    6C**   SIMFEL        IFGT      0
023400060130     C**                 MOVEL     §7LFSA        WSTOR             1
023500060130    6C**                 ENDIF
023600060130    5C**                 ENDIF
023700060130     c
023800060130    6C     SIMFEL        IFGT      0
023900060130     C                   MOVEL     §7LFSA        WSTOR             1
024000060130    6C                   ENDIF
024100941031     C* DETERMINO QUALE FLAG PRENDERE
024200970611    5C     D20TBO        IFEQ      'S'
024300960503     C     SIMFEL        OREQ      0
024400941031     C                   MOVEL     §7LFSS        WSTOR             1
024500970611    5C                   ENDIF
024600970611    4C                   ENDIF
024700941031     C*
024800970611    4C     WSTOR         IFEQ      'S'
024900941031     C                   CLEAR                   FNARBV00
025000941031     C                   Z-ADD     D20AAS        ARBAAS
025100941031     C                   Z-ADD     D20LNP        ARBLNP
025200941031     C                   Z-ADD     D20NRS        ARBNRS
025300941031     C                   Z-ADD     D20NSP        ARBNSP
025400941031     C                   MOVEL     D20CBO        ARBCBO
025500941031     C                   MOVEL     D20FVF        ARBFVC
025600941031     C                   Z-ADD     D20VLF        ARBVLC
025700941031     C                   TIME                    ARBORV
025800041217     C                   MOVEL     dateu         ARBDTV
025900941031     C                   MOVEL     WCVB          ARBCVB
026000941031     C                   WRITE     FNARBV00
026100970611    4C                   ENDIF
026200040429   4aC                   ENDIF
026300941025     C*
026400941025     C* ESEGUO LA SOSTITUZIONE
026500941031     C                   MOVEL     D20VLU        D20VLF
026600941025     C                   MOVEL     WFVF          D20FVF
026700941025    3C                   ENDIF
026800941025    2C                   ENDIF
026900941025    1C                   ENDIF
027000970818    0C                   ENDIF
027100941031     C*
027200041109     C                   ENDSR
027300941021     C*
027400941024     C* OPERAZIONI INIZIALI ------------------------------------------*
027500941024     C     *INZSR        BEGSR
027600941025     C     KTAB          KLIST
027700941025     C                   KFLD                    CODUT             1 0
027800941025     C                   KFLD                    COD               2
027900941025     C                   KFLD                    KEY               8
028000941025     C*
028100941031     C*
028200941031     C     *LIKE         DEFINE    D20LNP        WTFP
028300941031     C     *LIKE         DEFINE    ARBCVB        WCVB
028400941031     C     *LIKE         DEFINE    ARBCVB        WCVBS
028500040322     C     *LIKE         DEFINE    d20tbo        savtbo
028600941024     C                   ENDSR
028700941025     C*
028800941025     C* DETERMINO SE VOLUME TOTALE O PARIZIALE -----------------------*
028900941025     C     CTRVLC        BEGSR
029000941025     C                   SETOFF                                       0102
029100941025     C*
029200941025     C* VOLUME PARZIALE
029300050111     C     D20ncl        IFGT      D20NCR
029400941031     C                   MOVEL     'Z'           WFVF              1
029500941025     C                   SETON                                        01
029600941025     C                   ELSE
029700941025     C* VOLUME TOTALE
029800941025     C                   MOVEL     'T'           WFVF
029900941025     C                   SETON                                        02
030000941025     C                   ENDIF
030100941031     C*
030200941031     C                   MOVEL     WFVF          D20FVL
030300941025     C*
030400941025     C                   ENDSR
030500941025     C*
030600941025     C* DETERMINO CHI HA CML E CHI NO --------------------------------*
030700941025     C     CTRCML        BEGSR
030800941025     C                   CLEAR                   WSOST
030900941025     C                   CLEAR                   WINSE
031000941025     C                   CLEAR                   WINFI
031100941031     C*
031200941031     C* CHAIN DEL TIPO BOLLA
031300941031     C                   MOVEL     '3A'          COD
031400941031     C                   MOVEL(P)  D20CBO        KEY
031500941031     C     KTAB          CHAIN     TABEL                              30
031600941031     C  N30              MOVEL     TBLUNI        DS3A
031700941031     C  N30              MOVEL     §3ATB1        FLGTB1            1
031800941031     C   30              MOVEL     'F'           FLGTB1
031900941025     C*
032000941025     C* CARICO IN TABELLA LE DS DEI TIPI VOLUME CML "T" E "Z"
032100941031    1C     WCART         IFEQ      ' '
032200941025     C                   EXSR      CARTAB
032300941031     C                   MOVEL     '1'           WCART             1
032400941031    1C                   ENDIF
032500941025     C*
032600970826     C* A CAMBIO LINEA DI PARTENZA DETERMINO SE HO O MENO IL CML
032700970826     C     D20LNP        IFNE      SAVLNP
032800941025     C*
032900970826     C* REPERISCO IL TERMINAL PARTENZA :
033000941025     C                   Z-ADD     *ZEROS        PROV              3 0
033100970825     C                   CLEAR                   DSLV55
033200970825     C                   MOVEL     'P'           D55TPT
033300970825     C                   MOVEL     D20LNP        D55LIN
033400970825     C                   MOVEL     D20DTE        D55DRF
033500970825     C                   CALL      'FNLV55R'
033600970825     C                   PARM                    DSLV55
033700960503     C*
033800970825     C     D55ERR        IFEQ      ' '
033900970825     C                   Z-ADD     D55TFP        WTFP
034000970825     C                   ELSE
034100970825     C                   Z-ADD     D20LNP        WTFP
034200970825     C                   ENDIF
034300941025     C*
034400941025     C* CONTROLLO SE IL TERMINAL DI PARTENZA HA IL MACCHINONE CML
034500941025     C     WTFP          CHAIN     AZORG01L                           30
034600941025     C   30              MOVEL     *BLANKS       ORGDF0
034700040123     c                   eval      og150=orgdf0
034800040123     c                   if        §ogspv='S'
034900941025     C                   MOVEL     'S'           WCMLPA
035000941025     C                   ELSE
035100970826     C                   MOVEL     'N'           WCMLPA            1
035200941031    2C                   ENDIF
035300040503      * Cerco il network
035400040503     c                   Clear                   og143
035500040503     c     d20lnp        Chain     Azorg01l
035600040503     c                   If        %Found(Azorg01l)
035700040503     c                   Movel     orgde3        og143
035800040503     c                   EndIf
035900040503     c                   Eval      lnpntw = §ogntw
036000040503     c
036100970826     C                   MOVEL     D20LNP        SAVLNP
036200970826     C     *LIKE         DEFINE    D20LNP        SAVLNP
036300941031    1C                   ENDIF
036400030603
036500030603      * a cambio di linea arrivo cerco il network
036600030603     c                   If        d20lna <> savlna
036700030603     c                   Clear                   og143
036800030603     c     d20lna        Chain     Azorg01l
036900030603     c                   If        %Found(Azorg01l)
037000030603     c                   Movel     orgde3        og143
037100030603     c                   EndIf
037200030603     c                   Eval      lnantw = §ogntw
037300030603     c                   Eval      savlna = d20lna
037400030603     c                   EndIf
037500970826     C**
037600970826     C* CAUSALE VARIAZIONE
037700970826     C**
037800970826     C                   MOVEL     'VV'          WCVB
037900970826     C**
038000970826     C* FLAG SOSTITUZIONE
038100970826     C**
038200030603      * se network della lna è FedEx e volume da variare è "T"
038300030603      * devo sempre sostituire
038400030603
038500030603If  0c                   If        lnantw = 'FED' and wfvf = 'T'
038600030603     c                   Eval      wsost = 'S'
038700030603   x0c                   Else
038800030603
038900970826    1C     FLGTB1        IFEQ      'F'
039000970826    2C     WCMLPA        IFEQ      'S'
039100970826     C   01              MOVEL     Z7TAFA        WSOST             1
039200970826     C   02              MOVEL     T7TAFA        WSOST             1
039300970826     C                   ELSE
039400970826     C   01              MOVEL     Z7TAFN        WSOST             1
039500970826     C   02              MOVEL     T7TAFN        WSOST             1
039600970826    2C                   ENDIF
039700970826     C*
039800970826   X1C                   ELSE
039900970826     C*
040000970826    2C     WCMLPA        IFEQ      'S'
040100970826     C   01              MOVEL     Z7TAAA        WSOST             1
040200970826     C   02              MOVEL     T7TAAA        WSOST             1
040300970826     C                   ELSE
040400970826     C   01              MOVEL     Z7TAAN        WSOST             1
040500970826     C   02              MOVEL     T7TAAN        WSOST             1
040600970826    2C                   ENDIF
040700970826    1C                   ENDIF
040800970826     C**
040900970826    1C     WSOST         IFNE      ' '
041000970826     C                   MOVEL     '7T'          COD
041100970826     C                   MOVEL(P)  D20FVF        KEY
041200970826     C     KTAB          CHAIN     TABEL                              30
041300970826    2C     *IN30         IFEQ      *OFF
041400970826     C                   MOVEL     TBLUNI        DS7T
041500970826    3C     WFVF          IFEQ      'Z'
041600070615     c* Se SOZ è vuoto, prendo quelli dei franchi o assegnati
041700070615     c                   if        §7tsoz=' '
041800070615     c
041900070615     c                   if        flgtb1='F'
042000070615     C                   MOVEL     §7TSOZf       WSOST
042100070615     c                   else
042200070615     C                   MOVEL     §7TSOZa       WSOST
042300070615     c                   endif
042400070615     c*
042500070615     c                   else
042600970826     C                   MOVEL     §7TSOZ        WSOST
042700070615     c                   endif
042800070615     c
042900970826   X3C                   ELSE
043000070615     c
043100070615     c* Se SOT è vuoto, prendo quelli dei franchi o assegnati
043200070615     c                   if        §7tsot=' '
043300070615     c                   if        flgtb1='F'
043400070615     C                   MOVEL     §7TSOTf       WSOST
043500070615     c                   else
043600070615     C                   MOVEL     §7TSOTa       WSOST
043700070615     c                   endif
043800070615     c
043900070615     c                   else
044000970826     C                   MOVEL     §7TSOT        WSOST
044100070615     c                   endif
044200970826    3C                   ENDIF
044300970826     C                   CLEAR                   DS7T
044400970826    2C                   ENDIF
044500970826    1C                   ENDIF
044600030603
044700030603If  0c                   EndIf
044800040503c
044900040503     c* lnp euroexpress --> sostituisco sempre, anche REALE
045000040503     c                   if        lnpntw='EEX'
045100040503     c                   eval      wsost='S'
045200040503     c                   endif
045300970826     C**
045400970826     C* FALG DI TRASMISSIONE
045500970826     C**
045600970826     C*  PARTENZA
045700941025     C**
045800970902    0C     D20FTR        IFEQ      'S'
045900970902     C**
046000941031    1C     D20TBO        IFEQ      'P'
046100941025     C*
046200941031    2C     FLGTB1        IFEQ      'F'
046300941025     C* TRASMISSIONE IN SEDE
046400941031     C   01              MOVEL     Z7TPSF        WINSE             1
046500941031     C   02              MOVEL     T7TPSF        WINSE             1
046600041108     C* TRASMISSIONE IN parternza: non c'e' sono in partenza
046700941025     C*
046800941031   X2C                   ELSE
046900941025     C*
047000941025     C* TRASMISSIONE IN SEDE
047100941031     C   01              MOVEL     Z7TPSA        WINSE             1
047200941031     C   02              MOVEL     T7TPSA        WINSE             1
047300041108     C* TRASMISSIONE IN partenza: non c'e' sono in partenza
047400941031    2C                   ENDIF
047500941031    1C                   ENDIF
047600941025     C**
047700970826     C* ARRIVO
047800941025     C**
047900941025    1C     D20TBO        IFEQ      'A'
048000941025     C* FILIALE DI TRASMISSIONE
048100970826    2C     FLGTB1        IFEQ      'F'
048200970826     C* TRASMISSIONE IN SEDE
048300970826     C   01              MOVEL     Z7TASF        WINSE             1
048400970826     C   02              MOVEL     T7TASF        WINSE             1
048500041108     C* TRASMISSIONE IN partenza
048600970826     C   01              MOVEL     Z7TAPF        WINFI             1
048700970826     C   02              MOVEL     T7TAPF        WINFI             1
048800970826   X2C                   ELSE
048900941025     C* TRASMISSIONE IN SEDE
049000941031     C   01              MOVEL     Z7TASA        WINSE             1
049100941031     C   02              MOVEL     T7TASA        WINSE             1
049200041108     C* TRASMISSIONE IN partenza
049300941031     C   01              MOVEL     Z7TAPA        WINFI             1
049400941031     C   02              MOVEL     T7TAPA        WINFI             1
049500941025    2C                   ENDIF
049600041108     c* Trasmettere alla partenza
049700041108     c                   if        winfi='S'
049800041108     c                   eval      winfi='P'
049900041108     c                   endif
050000941025    1C                   ENDIF
050100941025     C**
050200941025     C* TRANSITO
050300941025     C**
050400941025    1C     D20TBO        IFEQ      'T'
050500941025     C*
050600941025    2C     FLGTB1        IFEQ      'F'
050700941025     C* TRASMISSIONE IN SEDE
050800941031     C   01              MOVEL     Z7TTSF        WINSE             1
050900941031     C   02              MOVEL     T7TTSF        WINSE             1
051000041108     C* TRASMISSIONE IN partenza
051100041108     C   01              MOVEL     Z7TTPF        WINFi             1
051200041108     C   02              MOVEL     T7TTPF        WINFi             1
051300941025     C*
051400941025   X2C                   ELSE
051500941025     C*
051600941025     C* TRASMISSIONE IN SEDE
051700941031     C   01              MOVEL     Z7TTSA        WINSE             1
051800941031     C   02              MOVEL     T7TTSA        WINSE             1
051900041108     C* TRASMISSIONE IN partenza
052000041108     C   01              MOVEL     Z7TTPA        WINFi             1
052100041108     C   02              MOVEL     T7TTPA        WINFi             1
052200941025     C*
052300941031    2C                   ENDIF
052400041108     c* Trasmettere alla partenza
052500041108     c                   if        winfi='S'
052600041108     c                   eval      winfi='P'
052700041108     c                   endif
052800941031    1C                   ENDIF
052900970902    0C                   ENDIF
053000941025     C*
053100941025     C                   ENDSR
053200941025     C*
053300941025     C* CARICO LE 2 DS DEI TIPI VOLUME ------------------------------*
053400941025     C     CARTAB        BEGSR
053500941031     C*
053600941031     C                   Z-ADD     1             CODUT             1 0
053700941031     C                   CALL      'X§PARUT'
053800941031     C                   PARM                    UT§DSE
053900941031     C                   MOVEL     REC80         CNCR80
054000941031     C                   MOVEL     RAGUT         RSUT             20
054100040505     C* SE KNSIF=*BLANKS IMPOSTO *LIBL
054200040505     C                   IF        KNSIF=*BLANKS
054300040505     C                   EVAL      KNSIF='*LIBL'
054400040505     C                   ENDIF
054500941031     C*
054600021008     C                   Z-ADD     48            LUNG
054700941025     C*
054800941025     C* CARICO FLAGS TIPO VOLUME 'T'
054900941025     C                   MOVEL     '7T'          COD
055000941025     C                   MOVEL(P)  'T'           KEY
055100941025     C     KTAB          CHAIN     TABEL00F                           32
055200941025     C*
055300941025     C  N32              MOVEL     TBLUNI        DS7T
055400941025     C   32              CLEAR                   DS7T
055500970826     C* SOSTITUZIONE VLS A VOL FRANCHI   CON LNP CON   CML
055600970826     C                   MOVEL     §7TAFA        T7TAFA            1
055700970826     C* SOSTITUZIONE VLS A VOL ASSEGNATI CON LNP CON   CML
055800970826     C                   MOVEL     §7TAAA        T7TAAA            1
055900970826     C* SOSTITUZIONE VLS A VOL FRANCHI   CON LNP SENZA CML
056000970826     C                   MOVEL     §7TAFN        T7TAFN            1
056100970826     C* SOSTITUZIONE VLS A VOL ASSEGNATI CON LNP SENZA CML
056200970826     C                   MOVEL     §7TAAN        T7TAAN            1
056300941025     C*
056400941025     C* ARR.:INVIO PARTEN FRANCHI
056500941025     C                   MOVEL     §7TAPF        T7TAPF            1
056600941025     C* ARR.:INVIO PARTEN ASSEGNATI
056700941025     C                   MOVEL     §7TAPA        T7TAPA            1
056800941025     C* ARR.:INVIO SEDE FRANCHI
056900941025     C                   MOVEL     §7TASF        T7TASF            1
057000941025     C* ARR.:INVIO SEDE ASSEGNATI
057100941025     C                   MOVEL     §7TASA        T7TASA            1
057200941025     C*
057300941025     C*
057400941025     C* PAR.:INVIO SEDE FRANCHI
057500941025     C                   MOVEL     §7TPSF        T7TPSF            1
057600941025     C* PAR.:INVIO SEDE ASSEGNATI
057700941025     C                   MOVEL     §7TPSA        T7TPSA            1
057800941025     C*
057900941025     C* TRAN:INVIO PARTEN FRANCHI
058000941025     C                   MOVEL     §7TTPF        T7TTPF            1
058100941025     C* TRAN:INVIO PARTEN ASSEGNATI
058200941025     C                   MOVEL     §7TTPA        T7TTPA            1
058300941025     C* TRAN:INVIO SEDE FRANCHI
058400941025     C                   MOVEL     §7TTSF        T7TTSF            1
058500941025     C* TRAN:INVIO SEDE ASSEGNATI
058600941025     C                   MOVEL     §7TTSA        T7TTSA            1
058700941025     C*
058800941025     C* CARICO FLAGS TIPO VOLUME 'Z'
058900941025     C                   MOVEL     '7T'          COD
059000941025     C                   MOVEL     *BLANKS       KEY
059100941025     C                   MOVEL     'Z'           KEY
059200941025     C     KTAB          CHAIN     TABEL00F                           32
059300941025     C*
059400941025     C  N32              MOVEL     TBLUNI        DS7T
059500941025     C   32              CLEAR                   DS7T
059600970826     C* SOSTITUZIONE VLS A VOL FRANCHI   CON LNP CON   CML
059700970826     C                   MOVEL     §7TAFA        Z7TAFA            1
059800970826     C* SOSTITUZIONE VLS A VOL ASSEGNATI CON LNP CON   CML
059900970826     C                   MOVEL     §7TAAA        Z7TAAA            1
060000970826     C* SOSTITUZIONE VLS A VOL FRANCHI   CON LNP SENZA CML
060100970826     C                   MOVEL     §7TAFN        Z7TAFN            1
060200970826     C* SOSTITUZIONE VLS A VOL ASSEGNATI CON LNP SENZA CML
060300970826     C                   MOVEL     §7TAAN        Z7TAAN            1
060400941025     C*
060500941025     C* ARR.:INVIO PARTEN FRANCHI
060600941025     C                   MOVEL     §7TAPF        Z7TAPF            1
060700941025     C* ARR.:INVIO PARTEN ASSEGNATI
060800941025     C                   MOVEL     §7TAPA        Z7TAPA            1
060900941025     C* ARR.:INVIO SEDE FRANCHI
061000941025     C                   MOVEL     §7TASF        Z7TASF            1
061100941025     C* ARR.:INVIO SEDE ASSEGNATI
061200941025     C                   MOVEL     §7TASA        Z7TASA            1
061300941025     C*
061400941025     C*
061500941025     C* PAR.:INVIO SEDE FRANCHI
061600941025     C                   MOVEL     §7TPSF        Z7TPSF            1
061700941025     C* PAR.:INVIO SEDE ASSEGNATI
061800941025     C                   MOVEL     §7TPSA        Z7TPSA            1
061900941025     C*
062000941025     C* TRAN:INVIO PARTEN FRANCHI
062100941025     C                   MOVEL     §7TTPF        Z7TTPF            1
062200941025     C* TRAN:INVIO PARTEN ASSEGNATI
062300941025     C                   MOVEL     §7TTPA        Z7TTPA            1
062400941025     C* TRAN:INVIO SEDE FRANCHI
062500941025     C                   MOVEL     §7TTSF        Z7TTSF            1
062600941025     C* TRAN:INVIO SEDE ASSEGNATI
062700941025     C                   MOVEL     §7TTSA        Z7TTSA            1
062800941025     C**
062900941025     C**
063000970826     C* SE SONO IN SEDE QUESTE ULTIME 3 COSE NON
063100941025     C*  LE FACCIO PERCHE' NON C'E' TRASMISSIONE
063200941025     C**
063300941025     C     D20TBO        IFNE      ' '
063400941025     C**
063500941025     C* LETTURA PERCENTUALE SCOSTAMENTO
063600941025     C**
063700941025     C                   MOVEL     '3I'          COD
063800941025     C                   MOVEL(P)  '1'           KEY
063900941025     C     KTAB          CHAIN     TABEL00F                           32
064000941025IF  1C     *IN32         IFEQ      '0'
064100941025     C                   MOVEL     TBLUNI        DS3I
064200941025E   1C                   ENDIF
064300971113     C                   ENDIF
064400941025     C                   ENDSR
064500941025     C*
064600941025     C* GESTIONE TRASMISSIONE VOLUME CML -----------------------------*
064700941025     C     GESTRA        BEGSR
064800941025     C*
064900971113     C                   SETOFF                                       06
065000941025     C**
065100041108     C* aggiorno in partenza
065200941025     C**
065300041108IF  1C     WINFI         IFne      ' '
065400941025     C*
065500941025     C* SE VOLUME TOTALE DA CML TRASMETTO SEMPRE
065600941025     C*
065700961014IF  2C     WFVF          IFEQ      'T'
065800941025     C                   SETON                                        06
065900941025     C*
066000941025     C* VOLUME PARZIALE DA CML
066100941025X   2C                   ELSE
066200941025     C*
066300941025     C* VOLUME NUOVO DEVE ESSERE MAGGIORE DEL VECCHIO
066400941031IF  3C     D20VLU        IFGT      D20VLF
066500941025     C*
066600941025     C* PULISCO CAMPI DI COMODO
066700941025     C                   Z-ADD     *ZEROS        COMVOL
066800941025     C                   Z-ADD     *ZEROS        VOLCON
066900941025     C*
067000941025     C* TRASFORMO IN VALORE ASSOLUTO LA PERCENTUALE
067100941231     C     D20VLF        MULT      §3IPSC        COMVOL            5 3
067200941231     C     COMVOL        DIV       100           COMVOL            5 3
067300941025     C*
067400941025     C* VOLUME VECCHIO + LA % DI SCOSTAMENTO
067500941231     C     D20VLF        ADD       COMVOL        VOLCON            5 3
067600941025     C*
067700941025     C* NUOVO VOLUME DEVE ESSERE MAGGIORE DEL VECCHIO + % SCOSTAMENTO
067800941031IF  4C     D20VLU        IFGT      VOLCON
067900941025     C                   SETON                                        06
068000941025E   4C                   ENDIF
068100941025E   3C                   ENDIF
068200941025E   2C                   ENDIF
068300941025     C*
068400041108     C* 06 ON - aggiorno in partenza
068500941031IF  2C     *IN06         IFEQ      *ON
068600041108     C                   movel     winfi         agvtbo
068700041108     C                   Z-ADD     D20AAS        agvAAS
068800041108     C                   Z-ADD     D20LNP        agvLNP
068900041108     C                   Z-ADD     D20NRS        agvNRS
069000041108     C                   Z-ADD     D20NSP        agvNSP
069100041108     C                   MOVEL     D20CBO        agvCBO
069200041108     C                   MOVEL     WFVF          agvFvC
069300041108     C                   Z-ADD     D20vLU        agvvlc
069400041108     C                   Z-ADD     D20nCr        agvNCr
069500041108     C                   TIME                    agvORV
069600041217     C                   MOVEL     dateu         agvDTV
069700041108     C                   MOVEL     WCVB          agvCVB
069800041108     c                   write     fnagv000
069900041108     c                   feod      fnagv00f
070000941025     C*
070100941025E   2C                   ENDIF
070200941025E   1C                   ENDIF
070300941031     C**
070400941025     C* INVIO A  S E D E
070500941031     C**
070600941025     C* FRANCHI O ASSEGNATI
070700941031IF  1C     WINSE         IFEQ      'S'
070800941025     C*
070900041108     C* Solo la prima volta apro il membro
071000041108    2c                   if        wapri=*blanks
071100941031     C                   EXSR      CAEXT
071200041108     c* 26 off - no errori
071300041108     c                   if        *in26=*off
071400041108     c                   eval      wapri='S'
071500041108     c                   else
071600041108     c                   eval      d20err='1'
071700041108     c                   endif
071800041108    2c                   endif
071900941025     C*
072000941025     C* SCRIVO SOLO SE HO APERTO IL MEMBRO
072100041108IF  2C                   IF        d20err=*blanks
072200941031     C                   Z-ADD     D20AAS        ARBAAS
072300941031     C                   Z-ADD     D20LNP        ARBLNP
072400941031     C                   Z-ADD     D20NRS        ARBNRS
072500941031     C                   Z-ADD     D20NSP        ARBNSP
072600941031     C                   MOVEL     D20CBO        ARBCBO
072700941031     C                   MOVEL     WFVF          ARBFVC
072800941031     C                   Z-ADD     D20VLU        ARBVLC
072900941031     C                   Z-ADD     D20NCR        ARBNCR
073000941031     C                   TIME                    ARBORV
073100041217     C                   MOVEL     dateu         ARBDTV
073200941031     C                   MOVEL     WCVB          ARBCVB
073300941031     C                   WRITE     FNARBVTT
073400041108    2c                   endif
073500041108    1c                   endif
073600941025     C*
073700941025     C                   ENDSR
073800941031     C*---------------------------------------------------------------*
073900941031     C*                    CAEXT                                      *
074000941031     C* ROUTINE DI APERTURA MEMBRO TRASMISSIONE                       *
074100941031     C*---------------------------------------------------------------*
074200941031     C     CAEXT         BEGSR
074300041109     c                   clear                   writenta
074400041108     c
074500041108     c* Verifico esistenza membro
074600040510     c                   EXSR      ADDmembro
074700041109     c     riopen        tag
074800941031     C* OVRDBF
074900041108     C                   MOVEL     *BLANKS       COMMAN           80
075000941031     C                   MOVEA     CMD4(1)       COMMAN
075100040510     C                   CALL      'QCMDEXC'
075200941031     C                   PARM                    COMMAN
075300041108     C                   PARM                    LUNG             15 5
075400941031     C*
075500041108     C                   OPEN      FNARBV0T                             26
075600041109     c                   if        writenta<2  and *in26
075700041109     c* se open non riuacita --> ritento con un dly
075800041109     C                   MOVEL     *BLANKS       COMMAN           80
075900041109     C                   MOVEA     CMDdly(1)     COMMAN
076000041109     C                   CALL      'QCMDEXC'
076100041109     C                   PARM                    COMMAN
076200041109     C                   PARM                    LUNG             15 5
076300041109     c
076400041109     c                   add       1             writenta          2 0
076500041109     c                   goto      riopen
076600041109     c                   endif
076700041109     c
076800941031     C*
076900941031     C                   ENDSR
077000040510     C*
077100040510     c* Vedo se manca il membro lo aggiungo  ------------------------*
077200040510     c     ADDMEMBRO     begsr
077300040510     C                   MOVEL     *BLANKS       FISICO           10
077400040510     C                   MOVEL     'FNARBV0T'    FISICO
077500040510     C                   MOVEL     *BLANKS       LOGICO           10
077600040510     C                   MOVEL     *BLANKS       LOGMBR           10
077700040510     C                   MOVEL     *BLANKS       FLG               1
077800040510     C*
077900040510     C                   CALL      'TRUL50C'
078000040510     C                   PARM                    FISICO
078100040510     C                   PARM                    MbrFis
078200040510     C                   PARM                    KNSIF
078300040510     C                   PARM                    LOGICO
078400040510     C                   PARM                    LOGMBR
078500040510     C                   PARM                    FLG
078600040510     c                   endsr
078700941031**         CMD4
078800041108OVRDBF FILE(FNARBV0T)               MBR(B946046)
078900041109**         CMDdly
079000041109DLYJOB (30)
