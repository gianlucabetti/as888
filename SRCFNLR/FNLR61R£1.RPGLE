000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200950104     H* FNLR61R *----------------------------------------------------*
000300920305     H*   ELENCO SPEDIZIONI ARRIVATE CON DATA SPEDIZIONE   DAL   AL
000400000000     H*--------------------------------------------------------------*
000500950104     FFNARB26L  IF   E           K DISK
000600950104     FFNART01L  IF   E           K DISK
000700060216     FFiAR401L  IF   E           K DISK
000800991105     FFIAR601L  IF   E           K DISK
000900051114     FFiAR901L  IF   E           K DISK
001000950104     FFNLBL01L  IF   E           K DISK
001100950104     F                                     RENAME(FNLBL000:FNLBL01)
001200950104     FFNLBL02L  IF   E           K DISK
001300900523     FTABEL00F  IF   E           K DISK
001400900523     FAZORG01L  IF   E           K DISK
001500991108     FFIARI01L  IF   E           K DISK
001600021204     FFIAPD01L  IF   E           K DISK
001700920306     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001800920306     F                                     USROPN
001900961230     F*
002000031218
002100080121     d WL1             s              3  0 Dim(30)
002200031218     d wpo             s                   Like(OrgFil)
002300031218     d xx              s              3  0
002400080121     d YY              s              3  0
002500031218
002600961230     D* DEFINIZIONE SCHIERE
002700991105     D TE              S             66    DIM(25) CTDATA PERRCD(1)             RIGHE STAMPA
002800920424     D SCN             S              7  0 DIM(10)                              SEGNAC NON CON
002900040830     D SK1             S              4  0 DIM(2000)                            COD PADRONCINO
003000040830     D DE1             S             14    DIM(2000)                            DECOD. PADRONC
003100920511     D SK2             S              1    DIM(50)                              CONS. PARTICOL
003200930128     D DE2             S             10    DIM(50)                              DECOD. CONSEGN
003300930223     D SK3             S              1    DIM(50)                              CONS. ANOMALE
003400930224     D DE3             S             15    DIM(50)                              DECOD. ANOMALE
003500930312     D AAN             S              2  0 DIM(20)                              ANNO  FIGLIA
003600930312     D LPN             S              3  0 DIM(20)                              LNP   FIGLIA
003700930312     D NRN             S              2  0 DIM(20)                              SERIE FIGLIA
003800930312     D NSN             S              7  0 DIM(20)                              SPED. FIGLIA
003900930312     D DSN             S              6  0 DIM(20)                              DATA  FIGLIA
004000930312     D LAN             S              3  0 DIM(20)                              LNA   FIGLIA
004100930312     D DEC             S             10    DIM(20)                              DECOD LNA FIGLIA
004200930427     D TVL             S              1    DIM(20)                              TIPI VOLUME
004300930427     D RVL             S              1    DIM(20)                              RAGGUP VOLUME
004400961230     D*
004500900523     D                 DS
004600950104     D  ARBAAS                 1      4  0
004700950104     D  ARBMGS                 5      8  0
004800950104     D  ARBDSP                 1      8  0
004900920424     D                 DS
005000950104     D  LBLAAN                 1      4  0
005100950104     D  LBLMGN                 5      8  0
005200950104     D  LBLDSN                 1      8  0
005300920424     D                 DS
005400950104     D  LBLAAP                 1      4  0
005500950104     D  LBLMGP                 5      8  0
005600950104     D  LBLDSP                 1      8  0
005700930129     D                 DS
005800950104     D  AA                     1      2  0
005900950105     D  MM                     3      4  0
006000950104     D  GG                     5      6  0
006100950104     D  DATA                   1      6  0
006200961127     D*
006300961127     D****
006400961127     D* PARAMETRI PASSATI DAL FILTRO                  - FNLR69R -
006500961127     D****
006600900523     D PARAM           DS
006700961127     D* DATA CONSEGNA   DAL/AL  (GG/MM/AAAA)
006800961127     D  VIDDBD                 1      8  0
006900961127     D  VIDDBA                 9     16  0
007000961127     D* DATA CONSEGNA   DAL/AL  (AAAA/MM/GG)
007100961127     D  COMDBD                17     24  0
007200961127     D  COMDBA                25     32  0
007300961127     D* LINEA ARRIVO
007400961127     D  VIDFIL                33     35  0
007500961127     D* VIDFLG = '1' --> SUDDIVISIONE STAMPA PER DATA SPEDIZIONE
007600961127     D  VIDFLG                36     36
007700961127     D* DATA SPEDIZIONE DAL/AL  (AAAA/MM/GG)
007800961127     D  COMDSD                37     44  0
007900961127     D  COMDSA                45     52  0
008000961127     D* LINEA DI PARTENZA
008100961127     D  VIDLNP                53     55  0
008200961213     D* DATA SPEDIZIONE DAL/AL  (GG/MM/AAAA)
008300961213     D  VIDDSD                56     63  0
008400961213     D  VIDDSA                64     71  0
008500961127     D*
008600920415     D CNCR80        E DS
008700080121     D og143         E DS
008800080121     D DS3A          E DS
008900930427     D DS7T          E DS
009000950105     D DSCV          E DS
009100950105     D DS5E          E DS
009200900511     D KPJBA         E DS
009300900523     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009400991105     D DSBS02        E DS                  EXTNAME(TIBS02DS)
009500991105     D YEUR          E DS                  EXTNAME(YEURCODS)
009600991105     D DGED          E DS
009700031218
009800031218     d Trul06ds      e ds
009900031218     d  L1                     1     90  0 Dim(30)
010000031218
010100930729     C*---------------------------------------------------------------*
010200930729     C* INDICATORI USATI                                              *
010300930729     C*---------------------------------------------------------------*
010400961127     C* 01    - IMMESSA DATA CONSEGNA DAL
010500961127     C* 02    - IMMESSA ANCHE DATA CONSEGNA AL
010600920415     C* 03    - STAMPO DATI GIACENZA MERCE
010700920415     C* 04    - STAMPO DATI CONSEGNA MERCE
010800910808     C* 05    - ESEGUO  1 VOLTA STAMPA DELLA 1 RIGA SEGNAC. NON CONSEC
010900961127     C* 06    - IMMESSA UNA SPECIFICA LINEA DI ARRIVO
011000920415     C* 07    - ESISTE RECORD GIACENZA
011100930728     C* 08    - IMMESSA DATA SPEDIZIONE 'DAL'
011200961127     C* 09    - OFF  RECORD DA ELABORARE
011300920415     C* 10    - GIACENTE   STAMPA SCRITTA
011400930128     C* 11    - STAMPO DATA LASCIATO AVVISO
011500920416     C* 12    - STAMPO DATA APERTURA GIACENZA
011600920416     C* 13    - STAMPO NUMERO GIACENZA
011700920416     C* 14    - STAMPO CODICE GIACENZA
011800920416     C* 15    - STAMPO DATI FATTURA
011900920416     C* 16    - STAMPO NOTA ACCREDITO
012000920416     C* 17    - STAMPO TIPO MANCATA CONSEGNA
012100920416     C* 18    - BLOCCO CONSEGNA = A   STAMPA SCRITTA : LASCIATO AVVISO
012200920416     C* 19    - BLOCCO CONSEGNA = B   STAMPA SCRITTA : BLOCCATA
012300920416     C* 20    - STAMPO DATA RISERVA CONSEGNA
012400900523     C* 21    - ESISTE VALORE MERCE
012500961127     C* 22    - IMMESSA UNA SPECIFICA LINEA DI PARTENZA
012600900523     C* 23    - ESISTE SERIE
012700920424     C* 25    - STAMPO DATI RELATIVI ALLA MAMMA
012800920424     C* 26    - STAMPO DATI RELATIVI ALLA FIGLIA
012900930127     C* 27    - OFF  STAMPO CODICE CONSEGNA ANOMALA
013000961213     C* 28    - IMMESSA ANCHE DATA SPEDIZIONE AL
013100031218      * 29    - Lookup £1
013200920306     C* 30    - LETTURA FINE FILE
013300900523     C* 31    - LETTURA FILE LINEA ARRIVO
013400920424     C* 32/34 - DI COMODO RIUTILIZZABILI
013500950105     C* 35/36/37  - LOKUP ANDATA A BUON FINE
013600930316     C* 60/70 - USATI PER CONDIZIONARE EXCPT DEI LEGAMI BOLLA
013700930316     C* 65    - NON FACCIO EXCPT FITTIZIA
013800900523     C* 90    - TROVATO ERRORE
013900930129     C* 91    - STAMPO NOMINATIVO RITIRANTE MERCE
014000930129     C* 92    - STAMPO NOTE
014100000731     C* 93    - STAMPO DATI RMB
014200991108     C* 94    - STAMPO ORA CONSEGNA MERCE
014300991108     C* 95    - STAMPO IMPORTI CON DECIMALI OFF-SENZA DECIMALI
014400930729     C*---------------------------------------------------------------*
014500000000     C     *ENTRY        PLIST
014600000000     C                   PARM                    KPJBA
014700950105     C                   MOVEL     KPJBU         PARAM
014800031218
014900031218      * se richieste tutte le linee di arrivo carico la £1
015000080121if  1c                   If        VidFil = *Zeros  or vidfil=999
015100031218     c                   Clear                   Trul06ds
015200031218     c                   Eval      D06Cod = '£1'
015300031218     c                   Movel     Simfel        D06Key
015400031218     c                   Movel(p)  Trul06ds      Kpjbu
015500031218     c                   Call      'TRUL06R'
015600031218     c                   Parm                    Kpjba
015700031218     c                   Movel     Kpjbu         Trul06ds
015800080121     c* se vidfil=999 carico solo le lna estere
015900080121     c                   if        vidfil=999
016000080121     c                   eval      wl1=l1
016100080121     c                   clear                   l1
016200080121     c                   z-add     1             xx
016300080121     c                   z-add     0             yy
016400080121     c                   dow       wl1(xx)>0
016500080121     c     wl1(xx)       chain     azorg01l
016600080121     c
016700080121     c                   if        %found(azorg01l)
016800080121     c                   movel     orgde3        og143
016900080121     c                   if        §ogntw='EEX' or §ogntw='DPD'
017000080121     c                             or §ogntw='FED'
017100080121     c
017200080121     c                   add       1             yy
017300080121     c                   movel     wl1(xx)       l1(yy)
017400080121     c                   endif
017500080121     c                   endif
017600080121     c
017700080121     c                   add       1             xx
017800080121     c                   enddo
017900080121     c                   endif
018000080121     c
018100080121      * se richista una linea di arrivo imposto la £1 con la lna passa ta
018200031218   x1c                   Else
018300031218     c                   Clear                   L1
018400031218     c                   Eval      L1(1) = VidFil
018500080121     c                   seton                                        06
018600031218    1c                   EndIf
018700930728     C*
018800930728     C* MI POSIZIONO SULLA DATA SPEDIZIONE
018900961127     C                   MOVEL     COMDSD        COMAAS
019000961127     C                   MOVE      COMDSD        COMMGS
019100930728     C*
019200961127     C* 08 ON  - IMMESSA DATA SPEDIZIONE 'DAL' A VIDEO
019300930917    1C     COMDSD        IFGT      *ZEROS
019400930728     C                   SETON                                        08
019500961213     C* 28 ON  - IMMESSA DATA SPEDIZIONE 'AL'  A VIDEO
019600961213    2C     COMDSA        IFNE      0
019700961213     C     COMDSD        ANDNE     COMDSA
019800961213     C                   SETON                                        28
019900961213    2C                   ENDIF
020000961213     C  N28              MOVEL     COMDSD        COMDSA
020100930917    1C                   ENDIF
020200920306     C*
020300920511     C* CARICO TABELLE CODICI PADRONCINO E CONSEGNE PARTICOLARI
020400920511     C                   EXSR      CARTAB
020500920511     C*
020600920306     C                   OPEN      PRTF198
020700971103     C                   SETON                                        OF
020800920306     C*
020900961127     C* CONTROLLO SE IMMESSA DATA CONSEGNA
021000961127    1C     COMDBD        IFGT      0
021100961127     C* 01 ON  - IMMESSA DATA CONSEGNA DAL
021200961127     C                   SETON                                        01
021300961127    2C     COMDBA        IFNE      0
021400920505     C     COMDBD        ANDNE     COMDBA
021500900523     C                   SETON                                        02
021600961127    2C                   ENDIF
021700920505     C  N02              MOVEL     COMDBD        COMDBA
021800961127    1C                   ENDIF
021900920316     C*
022000961127     C* 06 ON  - IMMESSA UNA SPECIFICA LINEA DI ARRIVO
022100080121     C***  VIDFIL        COMP      0                                  06
022200961127     C* SE E' STATA RICHIESTA LA STAMPA DELLE SOLE LINEE ARRIVO ESTERE
022300961127     C*   IMPOSTO VIDFIL IN MODO DA LEGGERE SOLO LINEE ESTERE
022400080121     C***06VIDFIL        IFEQ      999
022500080121     C***                SETOFF                                       06
022600080121     C***                Z-ADD     300           VIDFIL
022700080121     C***                ENDIF
022800961127     C*
022900961127     C* 22 ON  - IMMESSA UNA SPECIFICA LINEA DI PARTENZA
023000961127     C     VIDLNP        COMP      0                                  22
023100900523     C*
023200900523     C** ELABORAZIONE
023300080121     c                   z-add     1             xx
023400031218do  0c                   Do        30            xx
023500031218if a0c                   If        L1(xx) > *Zeros
023600031218     c                   Eval      VidFil = L1(xx)
023700900523     C                   EXSR      IMPO
023800930728     C*
023900930917    2C     *IN31         DOWEQ     *OFF
024000930917     C                   SETOFF                                       09
024100961127     C*
024200961127     C* SE LA DATA SPEDIZIONE E' MAGGIORE DELLA DATA CONSEGNA AL
024300961127     C*   ESCO DAL CICLO
024400961127    3C   01ARBDSP        IFGT      COMDBA
024500961127     C                   SETON                                        0931
024600961127    3C                   ENDIF
024700961127     C*
024800961127     C* SE IMMESSA CONTROLLO LA DATA SPEDIZIONE
024900961127     C  N09
025000961127    3CAN 08ARBDSP        IFLT      COMDSD
025100930917     C                   SETON                                        09
025200930917   X3C                   ELSE
025300961127    4C     ARBDSP        IFGT      COMDSA
025400930917     C                   SETON                                        0931
025500930917    4C                   ENDIF
025600930917    3C                   ENDIF
025700961127     C*
025800961127     C* SE IMMESSA CONTROLLO LA LINEA PARTENZA
025900961127     C  N09
026000961127    3CAN 22ARBLNP        IFNE      VIDLNP
026100961127     C                   SETON                                        09
026200961127    3C                   ENDIF
026300920228     C*
026400961127     C* SE IMMESSA CONTROLLO LA DATA CONSEGNA
026500961127     C  N09
026600961127    3CAN 01ARBDCM        IFLT      COMDBD
026700961127     C     ARBDCM        ORGT      COMDBA
026800961127     C                   SETON                                        09
026900961127    3C                   ENDIF
027000961127     C*
027100961127     C* 09 OFF -   R E C O R D    D A    E L A B O R A R E
027200961127    3C     *IN09         IFEQ      *OFF
027300920505     C*
027400930917     C* VIDLFG = '1' --> SUDDIVIDO STAMPE PER DATA SPEDIZIONE
027500930917    4C     VIDFLG        IFEQ      '1'
027600930917     C     ARBDSP        ANDNE     SAVDSP
027700920415     C                   CLOSE     PRTF198
027800920415     C                   OPEN      PRTF198
027900920505     C                   SETON                                        OF
028000950104     C                   Z-ADD     ARBDSP        SAVDSP            8 0
028100930917    4C                   END
028200920505     C* STAMPO SPEDIZIONE
028300920505     C                   EXSR      STAMPA
028400930917    3C                   ENDIF
028500910808     C*
028600950104     C  N31VIDFIL        READE     FNARB000                               31
028700930917    2C                   END
028800930729     C*
028900031218     C**!!!              SETON                                        31
029000920505     C*
029100920505     C* STAMPA TOTALI PER OGNI LINEA ARRIVO
029200920505     C                   EXSR      STATOT
029300930917     C*
029400930917     C* CHIUDO FILE E STAMPO NUOVA LIENA
029500930917     C                   CLOSE     PRTF198
029600930917     C                   OPEN      PRTF198
029700930917     C                   SETON                                        OF
029800920505     C*
029900920505     C* CONTROLLO SE SONO A FINE FILE O FINE LINEA
030000031218     C**N06              EXSR      IMPO
030100031218    1C**!!!              END
030200031218   a0c                   EndIf
030300031218    0c                   EndDo
030400920228     C*
030500900523     C                   SETON                                        LR
030600920316     C*
030700900523     C*--- IMPOSTO LETTURE LINEE -------------------------------------*
030800900523     C     IMPO          BEGSR
030900920505     C*
031000920505     C* LEGGO TUTTE LE LINEE DI ARRIVO
031100031218     C**!!!*IN06         IFEQ      *OFF
031200031218     C**!!!VIDFIL        SETGT     FNARB000
031300031218     C**!!!              READ      FNARB000                               31
031400031218     C*!N31              Z-ADD     ARBLNA        VIDFIL
031500031218     C**!!!              ENDIF
031600930728     C*
031700920505     C* LEGGO LA LINEA PASSATA
031800930917     C*  O LA LINEA TROVATA CON LA SETGT
031900031218     C**!!!*IN06         IFEQ      *ON
032000031218     C**!!!*IN06         OREQ      *OFF
032100031218     C**!!!*IN31         ANDEQ     *OFF
032200950104     C     KARB          SETLL     FNARB000
032300950104     C     VIDFIL        READE     FNARB000                               31
032400031218     C**!!!              ENDIF
032500920316     C*
032600920316     C* DECODIFICO LINEA DA STAMPARE
032700930917     C     *IN31         IFEQ      *OFF
032800930917     C     VIDFIL        CHAIN     AZORG                              32
032900930917     C  N32              MOVEL     ORGDES        DESFIL           25
033000930917     C   32              MOVEL     *BLANKS       DESFIL
033100930917     C                   ENDIF
033200920228     C*
033300900522     C                   ENDSR
033400920505     C*
033500920511     C* CARICO TABELLE -----------------------------------------------*
033600920511     C*
033700920511     C     CARTAB        BEGSR
033800920511     C*
033900950104     C* CARICO CODICI E DECODIFICA PADRONCINI
034000920511     C                   Z-ADD     1             B                 4 0
034100021204     C     'A'           SETLL     FIAPD01L
034200021204     C     'A'           READE     FIAPD01L                               32
034300920511     C     *IN32         DOWEQ     '0'
034400031218     c                   Movel     ApdPdr        wpo
034500031218     c     wpo           Lookup    L1                                     29
034600031218     c                   If        *In29 = *On
034700950104     C                   MOVE      APDPDR        SK1(B)
034800950104     C                   MOVEL     APDRSC        DE1(B)
034900920511     C                   ADD       1             B
035000031218     c                   EndIf
035100021204     C     'A'           READE     FIAPD01L                               32
035200920511     C                   END
035300920511     C*
035400920511     C* CARICO TABELLE CONSEGNE PARTICOLARI E DECODIFICA CONSEGNE
035500920511     C                   MOVEL     '1P'          COD
035600920511     C                   Z-ADD     1             C                 4 0
035700920511     C     KTAB2         SETLL     TABEL
035800920511     C     KTAB2         READE     TABEL                                  32
035900920511     C     *IN32         DOWEQ     '0'
036000920511     C     TBLFLG        IFEQ      *BLANKS
036100920511     C                   MOVEL     TBLKEY        SK2(C)
036200920511     C                   MOVEL     TBLUNI        DE2(C)
036300920512     C                   ADD       1             C
036400920511     C                   END
036500920511     C     KTAB2         READE     TABEL                                  32
036600920511     C                   END
036700920511     C*
036800930223     C* CARICO TABELLE CONSEGNE ANOMALE E DECODIFICA CONSEGNE
036900930223     C                   MOVEL     '7O'          COD
037000930223     C                   Z-ADD     1             D                 4 0
037100930223     C     KTAB2         SETLL     TABEL
037200930223     C     KTAB2         READE     TABEL                                  32
037300930223     C     *IN32         DOWEQ     '0'
037400930223     C     TBLFLG        IFEQ      *BLANKS
037500930223     C                   MOVEL     TBLKEY        SK3(D)
037600930223     C                   MOVEL     TBLUNI        DE3(D)
037700930223     C                   ADD       1             D
037800930223     C                   END
037900930223     C     KTAB2         READE     TABEL                                  32
038000930223     C                   END
038100930427     C*
038200930427     C* CARICO TABELLA TIPI VOLUME
038300930427     C                   MOVEL     '7T'          COD
038400930427     C                   Z-ADD     0             Z                 3 0
038500930427     C     KTAB2         SETLL     TABEL
038600930427     C     KTAB2         READE     TABEL                                  32
038700930427     C*
038800930427     C     *IN32         DOWEQ     '0'
038900930427     C     TBLFLG        IFEQ      ' '
039000930427     C                   ADD       1             Z
039100930427     C                   MOVEL     TBLKEY        TVL(Z)
039200930427     C                   MOVEL     TBLUNI        DS7T
039300930427     C                   MOVEL     §7TRVL        RVL(Z)
039400930427     C                   END
039500930427     C*
039600930427     C     KTAB2         READE     TABEL                                  32
039700930427     C                   END
039800930223     C*
039900920511     C                   ENDSR
040000920511     C*
040100920511     C* STAMPO SPEDIZIONE --------------------------------------------*
040200920505     C     STAMPA        BEGSR
040300930917     C                   SETOFF                                       030407
040400930917     C                   SETOFF                                       101112
040500930917     C                   SETOFF                                       1314
040600930917     C                   SETOFF                                       171819
040700930917     C                   SETOFF                                       2032
040800930917     C                   SETOFF                                       353627
040900950105     C                   SETOFF                                       37
041000930917     C*
041100930917     C* 04 ON - STAMPO DATI CONSEGNA
041200930917     C                   SETON                                        04
041300930917     C*
041400920505     C* DECODIFICO TIPO BOLLA
041500920505     C                   MOVEL     '3A'          COD
041600920505     C                   MOVEL     *BLANKS       KEY
041700950104     C                   MOVEL     ARBCBO        KEY
041800991105     C     KTAB          CHAIN     TABEL                              32
041900991105     C   32              CLEAR                   DS3A
042000920505     C  N32              MOVEL     TBLUNI        DS3A
042100991105     C                   MOVEL     §3ADES        STACBL            6
042200991105     C                   MOVEL     §3ATB1        FLGTB1            1
042300920505     C*
042400920505     C* TOTALI GENERALI PER LINEA DI ARRIVO
042500920505     C* SPEDIZIONI
042600920505     C                   ADD       1             NUSPE             7 0
042700920505     C* COLLI
042800920505     C                   ADD       ARBNCL        NCOLLI            7 0
042900920505     C* PESI
043000950105     C                   Z-ADD     ARBPKF        STAPKF            6 1
043100950105     C*
043200950104     C                   ADD       ARBPKF        TPESO            10 1
043300950104     C     ARBPKF        IFGT      3500
043400950104     C                   ADD       ARBPKF        O35              10 1
043500920505     C                   ELSE
043600950104     C                   ADD       ARBPKF        F35              10 1
043700920505     C                   END
043800920505     C* VOLUMI
043900930427     C                   Z-ADD     1             Z
044000950104     C     ARBFVF        LOOKUP    TVL(Z)                                 32
044100930427     C  N32              Z-ADD     20            Z
044200930427     C*
044300930427     C*
044400930427     C     RVL(Z)        IFEQ      'R'
044500950104     C                   ADD       ARBVLF        VRET              8 2
044600920505     C                   ELSE
044700920505     C*
044800930427     C     RVL(Z)        IFEQ      'P'
044900950104     C                   ADD       ARBVLF        VPRT              8 2
045000920505     C*
045100920505     C                   ELSE
045200950104     C                   ADD       ARBVLF        VAUT              8 2
045300920505     C                   END
045400920505     C                   END
045500920505     C*
045600920505     C                   MOVEL     ARBRSM        STARSM           24
045700950104     C                   MOVEL     ARBRSD        STARSD           25
045800920505     C                   MOVEL     ARBINM        STAINM           24
045900920505     C                   MOVEL     ARBIND        STAIND           24
046000920505     C*
046100991105     C* ESISTENZA IMPORTO DA ASSICURARE
046200991105     C                   MOVE      *ZEROS        ST0IAS           10 0
046300991105     C                   MOVE      *ZEROS        STDIAS            9 3
046400950104     C     ARBIAS        COMP      0                                  21
046500991105    1C     *IN21         IFEQ      '1'
046600011025    2C     ARBVAS        IFNE      §GEDCN
046700011025      *
046800011025     C                   CLEAR                   YEUR
046900991105     C                   MOVEL     ARBVAS        YECDVI
047000011025     C                   MOVEL     §GEDCN        YECDVO
047100991105     C                   Z-ADD     ARBIAS        YECIMI
047200011025     C                   MOVE      §CVDEC        YECDCO
047300991105     C*
047400991105     C                   CALL      'YEURCO'
047500991105     C                   PARM                    YEUR
047600991105     C*
047700991105    3C     YECESI        IFEQ      ' '
047800991105     C   95              Z-ADD     YECIMO        STDIAS
047900991105     C  N95              Z-ADD     YECIMO        ST0IAS
048000991105    3C                   ENDIF
048100991105   X2C                   ELSE
048200991105     C   95              Z-ADD     ARBIAS        STDIAS
048300991105     C  N95              Z-ADD     ARBIAS        ST0IAS
048400991105    2C                   END
048500991105    1C                   END
048600920505     C*
048700920505     C* ESPRESSO
048800090604     c                   clear                   ds5e
048900950104     C                   MOVEL     '5E'          COD
049000950104     C                   MOVEL     *BLANKS       KEY
049100950104     C                   MOVEL     ARBTSP        KEY
049200950104     C     KTAB          CHAIN     TABEL                              32
049300950104     C  N32              MOVEL     TBLUNI        DS5E
049400090604     C  N32              MOVEL     §5ED08        STADES            8
049500950104     C   32              MOVEL     *BLANKS       STADES
049600920505     C*
049700920505     C* SERIE
049800920505     C     ARBNRS        COMP      0                                  2323
049900920505     C*
050000920505     C* ASSEGNATI
050100920505     C                   MOVE      ARBKSC        COMKSC            4 0
050200920505     C     COMKSC        IFEQ      9999
050300920505     C                   MOVEL     ARBLNA        ARBKSC
050400920505     C                   END
050500920505     C*
050600920505     C* GIRO DATA SPEDIZIONE
050700950104     C                   MOVE      ARBDSP        DATA
050800950104     C                   MOVE      ARBDSP        VIDDSP            6 0
050900930129     C                   MOVEL     GG            VIDDSP
051000930129     C                   MOVE      AA            VIDDSP
051100920505     C*
051200920505     C* GIRO DATA ARRIVO MERCE A FILIALE ARRIVO
051300950104     C                   MOVE      ARBDAM        DATA
051400950104     C                   MOVE      MM            VIDDAM            4 0
051500930129     C                   MOVEL     GG            VIDDAM
051600991105     C*
051700950104     C                   MOVE      *ZEROS        AR6NFT
051800950104     C                   MOVE      *ZEROS        AR6DFT
051900991105     C                   MOVE      *ZEROS        AR6DIV
052000950104     C                   MOVE      *ZEROS        AR6IMV
052100991105     C                   MOVE      *ZEROS        ST0IMV           10 0
052200991105     C                   MOVE      *ZEROS        STDIMV            9 3
052300991108     C                   MOVE      *ZEROS        ST0IPP           10 0
052400991108     C                   MOVE      *ZEROS        STDIPP            9 3
052500950109     C                   MOVE      *ZEROS        STADFT
052600950109     C                   MOVE      *ZEROS        STANFT
052700950109     C                   SETOFF                                       1516
052800991105   1AC     FLGTB1        IFEQ      'A'
052900991105     C                   MOVEL     '1'           KTRC
053000991105     C                   EXSR      TASSAZ
053100991105  X1AC                   ELSE
053200920505     C*
053300950109    2C     §3ATB2        IFNE      *BLANKS
053400991105     C                   MOVEL     '2'           KTRC
053500991105     C                   EXSR      TASSAZ
053600930917    2C                   END
053700991105    1C                   END
053800920505     C*
053900920505     C* NOTA ACCREDITO
054000930917    2C     ARBFNC        IFEQ      'S'
054100920505     C                   SETON                                        16
054200930917    2C                   END
054300920505     C*
054400920505     C*
054500920505     C* GIRO DATA CONSEGNA MERCE
054600950104     C                   MOVE      ARBDCM        DATA
054700950104     C                   MOVE      MM            VIDDCM            4 0
054800930129     C                   MOVEL     GG            VIDDCM
054900920505     C*
055000920505     C* GIRO DATA DISTINTA CONSEGNA
055100950104     C                   MOVE      ARBDDC        DATA
055200950104     C                   MOVE      MM            VIDDDC            4 0
055300930129     C                   MOVEL     GG            VIDDDC
055400920505     C*
055500920511     C* CODICE PADRONCINO CONSEGNA
055600920512     C                   Z-ADD     1             B
055700950105     C                   MOVE      ARBPDC        COMPDC            4 0
055800950104     C     COMPDC        LOOKUP    SK1(B)                                 35
055900920511     C*
056000920511     C* CONSEGNE PARTICOLARI/TIPO CONSEGNA
056100950105     C                   MOVE      *BLANKS       ST1SK2            1
056200950105     C                   MOVE      *BLANKS       ST1DE2           10
056300950104    2C     ARBTC1        IFNE      ' '
056400920512     C                   Z-ADD     1             C
056500950104     C     ARBTC1        LOOKUP    SK2(C)                                 36
056600950105     C   36              MOVEL     SK2(C)        ST1SK2
056700950105     C   36              MOVEL     DE2(C)        ST1DE2
056800930917    2C                   END
056900950105     C*
057000950105     C                   MOVE      *BLANKS       ST2SK2            1
057100950105     C                   MOVE      *BLANKS       ST2DE2           10
057200950105    2C     ARBTC2        IFNE      ' '
057300950105     C                   Z-ADD     1             C
057400950105     C     ARBTC2        LOOKUP    SK2(C)                                 37
057500950105     C   37              MOVEL     SK2(C)        ST2SK2
057600950105     C   37              MOVEL     DE2(C)        ST2DE2
057700950105    2C                   END
057800930127     C*
057900930127     C* GIRO DATA 1a CONSEGNA PARZIALE
058000950104     C                   MOVE      ARBDCP        DATA
058100950104     C                   MOVE      MM            STADCP            4 0
058200930129     C                   MOVEL     GG            STADCP
058300930127     C*
058400930127     C* DECODIFICO CODICE CONSEGNA ANOMALA
058500930917    2C     ARBCCA        IFNE      ' '
058600930223     C                   Z-ADD     1             D
058700930223     C     ARBCCA        LOOKUP    SK3(D)                                 27
058800930917    2C                   END
058900930127     C*
059000991108     C* STAMPO TOTALE PARZIALE INCASSATO
059100991108     C                   CLEAR                   WCONVE
059200991108     C     KARI          SETLL     FIARI000
059300991108     C     KARI          READE     FIARI000                               37
059400991108    1C     *IN37         DOWEQ     *OFF
059500991108     C     ARIIMP        ADD       ARIIPP        WIPP
059600991108    2C     WIPP          IFGT      0
059700011025    3C     ARIDIV        IFNE      §GEDCN
059800991108     C**
059900991108     C                   CLEAR                   YEUR
060000991108     C                   MOVEL     ARIDIV        YECDVI
060100011025     C                   MOVEL     §GEDCN        YECDVO
060200991108     C                   Z-ADD     WIPP          YECIMI
060300011025     C                   MOVE      §CVDEC        YECDCO
060400991108     C*
060500991108     C                   CALL      'YEURCO'
060600991108     C                   PARM                    YEUR
060700991108     C*
060800991108    4C     YECESI        IFEQ      ' '
060900991108     C                   ADD       YECIMO        WCONVE
061000991108    4C                   ENDIF
061100991108   X3C                   ELSE
061200991108     C                   ADD       WIPP          WCONVE
061300991108    3C                   ENDIF
061400991108    2C                   ENDIF
061500991108     C**
061600991108     C     KARI          READE     FIARI000                               37               I
061700991108    1C                   ENDDO
061800991108     C**
061900991108     C  N95              Z-ADD     WCONVE        ST0IPP
062000991108     C   95              Z-ADD     WCONVE        STDIPP
062100950104     C* RICERCA NOTE
062200950104     C                   SETOFF                                       919293
062300950104     C                   MOVEL     *BLANKS       S91NTC
062400950104     C                   MOVEL     *BLANKS       S92NTC
062500950104     C                   MOVEL     *BLANKS       S94NTC
062600060216     C     KARB2         SETLL     FiAR401L
062700060216     C     KARB2         READE     FiAR401L                               32
062800950104    2C     *IN32         DOWEQ     '0'
062900950104     C* NOTE
063000950104     C     AR4TRC        IFEQ      '0'
063100950104     C                   SETON                                        92
063200950104     C                   MOVEL     AR4NOT        S92NTC           35
063300950104     C                   ELSE
063400950104     C* RITIRANTE MERCE
063500950104     C     AR4TRC        IFEQ      '1'
063600950104     C                   SETON                                        91
063700950104     C                   MOVEL     AR4NOT        S91NTC           35
063800950104     C                   ELSE
063900950104     C* NOTE RIMBORSO
064000950104     C     AR4TRC        IFEQ      '2'
064100950104     C                   SETON                                        93
064200950104     C                   ELSE
064300950104     C* NOTE PARTENZA 1
064400950104     C     AR4TRC        IFEQ      '8'
064500950104     C                   MOVEL     AR4NOT        S94NTC           35
064600950104     C                   END
064700950104     C                   END
064800950104     C                   END
064900950104     C                   END
065000060216     C     KARB2         READE     FiAR401L                               32
065100950104     C                   ENDDO
065200950105     C* IMPORTO CONTRASSEGNO
065300991105     C                   MOVE      *ZEROS        ST0CAS           10 0
065400991105     C                   MOVE      *ZEROS        STDCAS            9 3
065500051114     C     KARB2         CHAIN     FiAR901L                           32
065600991105    1C     *IN32         IFEQ      '0'
065700011025    2C     AR9VCA        IFNE      §GEDCN
065800011025      *
065900991105     C                   CLEAR                   YEUR
066000991105     C                   MOVEL     AR9VCA        YECDVI
066100011025     C                   MOVEL     §GEDCN        YECDVO
066200991105     C                   Z-ADD     AR9CAS        YECIMI
066300011025     C                   MOVE      §CVDEC        YECDCO
066400991105     C*
066500991105     C                   CALL      'YEURCO'
066600991105     C                   PARM                    YEUR
066700991105     C*
066800991105    3C     YECESI        IFEQ      ' '
066900991105     C   95              Z-ADD     YECIMO        STDCAS
067000991105     C  N95              Z-ADD     YECIMO        ST0CAS
067100991105    3C                   ENDIF
067200991105   X2C                   ELSE
067300991105     C   95              Z-ADD     AR9CAS        STDCAS
067400991105     C  N95              Z-ADD     AR9CAS        ST0CAS
067500991105    2C                   END
067600991105    1C                   END
067700950105     C* LOCALITA' MITTENTE
067800950105     C                   MOVE      *BLANKS       STALOM           20
067900950105     C                   MOVEL     ARBLOM        STALOM
068000950105     C* LOCALITA' DESTINATARIO
068100950105     C                   MOVE      *BLANKS       STALOD           20
068200950105     C                   MOVEL     ARBLOD        STALOD
068300991108     C**
068400991108     C* SE IL COD AUTOTRASPORTATORE E' = 0 NON LO STAMPO
068500991108     C     SK1(B)        IFEQ      0
068600991108     C                   SETOFF                                       35
068700991108     C                   ENDIF
068800991108     C*
068900991108     C     ARBHMC        COMP      0                                  94
069000930129     C*
069100920505     C**** STAMPO TESTATA
069200920505     C   OF              EXCEPT    TESTA
069300920505     C   OF              SETOFF                                       OF
069400920505     C**** STAMPO DETTAGLIO
069500920505     C                   EXCEPT    DET
069600930128     C*
069700920505     C****
069800920505     C* CONTROLLO SE MAMMA O FIGLIA
069900920505     C                   SETOFF                                       2526
070000930129     C                   SETOFF                                       23
070100930129     C*
070200950104     C     KARB2         CHAIN     FNLBL01                            33
070300920505     C     *IN33         IFEQ      '0'
070400920505     C* GIRO DATA SPEDIZIONE MAMMA
070500950104     C                   MOVE      LBLDSP        DATA
070600950104     C                   MOVE      LBLDSP        STADSP            6 0
070700930129     C                   MOVEL     GG            STADSP
070800930129     C                   MOVE      AA            STADSP
070900930129     C* PRENDO DECODIFICA DELLA LINEA ARRIVO
071000930129     C     LBLLAP        CHAIN     AZORG                              32
071100930129     C  N32              MOVEL     ORGDES        DESLNA           10
071200930129     C   32              MOVEL     *BLANKS       DESLNA
071300920505     C*
071400920505     C                   SETON                                        25
071500930129     C                   END
071600930129     C*
071700930129     C  N25              EXSR      LEGSEG
071800930129     C*****
071900930129     C* STAMPO NOTE - RITIRANTE - RIMBORSO - LEGAMI BOLLA
072000930316     C                   SETOFF                                       607065
072100930312     C                   Z-ADD     1             X
072200930312     C*
072300930312     C   91              EXCEPT    RIT                                          *RITIRANTE
072400930312     C*
072500930917    1C     X             DOWLE     20
072600930312     C*
072700930312     C  N91
072800930312     CAN 25
072900930312     CANN26              EXCEPT    LEGA                                         *BOLLA MAMMA
073000930316     C*
073100930917     C  N91
073200930917     CAN 25
073300930917    2CAN 26AAN(X)        IFGT      0
073400930917    3C   OF              DO
073500930316     C                   EXCEPT    TESTA
073600930316     C                   EXCEPT    FIT2
073700930316     C                   SETOFF                                       OF
073800930917    3C                   END
073900930316     C*
074000930312     C                   EXCEPT    LEGA                                         *BOLLA FIGLIA
074100930312     C   60              EXCEPT    FIT
074200930312     C  N60
074300930312     CAN 92
074400930312     CAN 93              SETON                                        70
074500930316     C*
074600930316     C  N92
074700930316     CANN93
074800930316     COR 92
074900930316     CANN93
075000930316     CORN92
075100930316     CAN 93              SETON                                        65
075200930917    2C                   END
075300930312     C*
075400930312     C  N60
075500020517     CAN 92              EXCEPT    NOTe                                         *NOTE
075600930312     C*
075700930312     C   70              ADD       1             X
075800930917    2C   70AAN(X)        IFGT      0
075900930312     C                   EXCEPT    LEGA                                         *BOLLA FIGLIA
076000930917    2C                   END
076100930312     C   70              SETOFF                                       70
076200930129     C*
076300930917     C  N60
076400930917     CANN92
076500930917    2CANN93*IN91         IFEQ      '1'
076600930129     C     *IN91         OREQ      '0'
076700930129     C     *IN25         ANDEQ     '1'
076800930129     C                   EXCEPT    FIT
076900930917    2C                   END
077000930129     C*
077100930312     C   91
077200930312     CAN 25
077300930312     CANN26              EXCEPT    LEGA                                         *BOLLA MAMMA
077400930316     C*
077500930917     C   91
077600930917     CAN 25
077700930917    2CAN 26AAN(X)        IFGT      0
077800930917    3C   OF              DO
077900930316     C                   EXCEPT    TESTA
078000930316     C                   EXCEPT    FIT2
078100930316     C                   SETOFF                                       OF
078200930917    3C                   END
078300930316     C*
078400930312     C                   EXCEPT    LEGA                                         *BOLLA FIGLIA
078500930312     C   60              EXCEPT    FIT
078600930917    2C                   END
078700930312     C*
078800930129     C  N91
078900930129     CAN 25
079000930129     CANN26              DO
079100930129     C                   EXSR      LEGSEG
079200930312     C   26              EXCEPT    LEGA                                         *BOLLA FIGLIA
079300930129     C                   END
079400930129     C*
079500000731     C  N60*IN92         IFNE      '1'
079600000731     C     *IN93         ORNE      '1'
079700930129     C   91
079800930129     CAN 25              EXCEPT    FIT
079900930316     C  N91
080000930316     CAN 25
080100930316     CAN 26
080200930316     CANN65              EXCEPT    FIT
080300930129     C                   END
080400930129     C*
080500930129     C   91
080600930129     CAN 25
080700930129     CANN26              DO
080800930129     C                   EXSR      LEGSEG
080900930312     C   26              EXCEPT    LEGA                                         *BOLLA FIGLIA
081000930129     C   26              EXCEPT    FIT
081100930129     C                   END
081200930129     C*
081300930312     C   26              ADD       1             X
081400930312     C   26AAN(X)        IFEQ      0
081500930312     C                   ADD       20            X
081600930312     C                   END
081700930312     C*
081800930312     C   26              SETON                                        60
081900930312     C  N26              ADD       20            X
082000930312     C                   END
082100930312     C*
082200920505     C* CONTROLLO SE SEGNACOLLI NON CONSECUTIVI
082300920505     C     ARBFNS        IFEQ      'S'
082400920505     C                   EXSR      STAART
082500920505     C                   END
082600920505     C                   ENDSR
082700991105     C*
082800991105     C*--- STAMPO LEGAME BOLLA SEGUENTE ------------------------------*
082900991105     C*
083000991105     C     TASSAZ        BEGSR
083100991105     C     KAR6          CHAIN     FIAR601L                           32
083200991105    1C     *IN32         IFEQ      '0'
083300991105     C*
083400991105     C* DATA E NUMERO FATTURA
083500991105    2C     AR6DFT        IFGT      0
083600991105     C                   SETON                                        15
083700991105     C* GIRO DATA FATTURA
083800991105     C                   MOVE      AR6DFT        DATA
083900991105     C                   MOVE      MM            STADFT            4 0
084000991105     C                   MOVEL     GG            STADFT
084100991105     C* NUMERO FATTURA
084200991105     C                   MOVE      AR6NFT        STANFT            6 0
084300991105    2C                   END
084400991105     C**
084500991105     C                   MOVE      AR6IMV        W0030             3 0
084600991105    2C     AR6IMV        IFGT      0
084700011025    3C     AR6DIV        IFNE      §GEDCN
084800011025      *
084900991105     C                   MOVEL     AR6DIV        YECDVI
085000011025     C                   MOVEL     §GEDCN        YECDVO
085100991105     C                   Z-ADD     AR6IMV        YECIMI
085200011025     C                   MOVE      §CVDEC        YECDCO
085300991105     C*
085400991105     C                   CALL      'YEURCO'
085500991105     C                   PARM                    YEUR
085600991105     C*
085700991105    4C     YECESI        IFEQ      ' '
085800991105     C   95              Z-ADD     YECIMO        STDIMV
085900991105     C  N95              Z-ADD     YECIMO        ST0IMV
086000991105    4C                   ENDIF
086100991105   X3C                   ELSE
086200991105     C   95              Z-ADD     AR6IMV        STDIMV
086300991105     C  N95              Z-ADD     AR6IMV        ST0IMV
086400991105    3C                   END
086500991105    2C                   END
086600991105    1C                   END
086700991105     C                   ENDSR
086800910808     C*
086900930129     C*--- STAMPO LEGAME BOLLA SEGUENTE ------------------------------*
087000930129     C*
087100930129     C     LEGSEG        BEGSR
087200930312     C                   MOVEA     *ZEROS        AAN
087300930312     C                   MOVEA     *ZEROS        LPN
087400930312     C                   MOVEA     *ZEROS        NRN
087500930312     C                   MOVEA     *ZEROS        NSN
087600930312     C                   MOVEA     *ZEROS        DSN
087700930312     C                   MOVEA     *ZEROS        LAN
087800930312     C                   MOVEA     *ZEROS        DEC
087900930312     C                   Z-ADD     1             A                 3 0
088000930312     C*
088100950104     C     KARB2         SETLL     FNLBL000
088200950104     C     KARB2         READE     FNLBL000                               34
088300930312     C*
088400930312     C     *IN34         DOWEQ     '0'
088500930312     C                   SETON                                        2526
088600930312     C*
088700950104     C                   MOVE      LBLAAN        AAN(A)
088800930312     C                   MOVEL     LBLLPN        LPN(A)
088900930312     C                   MOVEL     LBLNRN        NRN(A)
089000930312     C                   MOVEL     LBLNSN        NSN(A)
089100930129     C* GIRO DATA SPEDIZIONE FIGLIA
089200950104     C                   MOVE      LBLDSN        DATA
089300950104     C                   MOVE      LBLDSN        DSN(A)
089400930312     C                   MOVEL     GG            DSN(A)
089500930312     C                   MOVE      AA            DSN(A)
089600930312     C*
089700930312     C                   MOVEL     LBLLAN        LAN(A)
089800930129     C* PRENDO DECODIFICA DELLA LINEA ARRIVO
089900930129     C     LBLLAN        CHAIN     AZORG                              32
090000930312     C  N32              MOVEL     ORGDES        DEC(A)
090100930312     C   32              MOVEL     *BLANKS       DEC(A)
090200930129     C*
090300930312     C                   ADD       1             A
090400930312     C     A             COMP      20                                     34
090500930312     C*
090600950104     C  N34KARB2         READE     FNLBL000                               34
090700930129     C                   END
090800930129     C                   ENDSR
090900930312     C*
091000930129     C*--- STAMPO SEGNACOLLI NON CONSECUTIVI -------------------------*
091100930129     C*
091200920413     C     STAART        BEGSR
091300920413     C                   MOVEA     *ZEROS        SCN
091400920413     C                   Z-ADD     1             X                 2 0
091500920413     C                   SETOFF                                       05
091600920413     C*
091700950104     C     KARB2         SETLL     FNART01L
091800950104     C     KARB2         READE     FNART01L                               32
091900920413     C*
092000920413     C     *IN32         DOWEQ     '0'
092100950104     C                   MOVEL     ARTNSC        SCN(X)
092200920413     C*
092300920413     C                   ADD       1             X
092400920413     C*
092500920413     C* STAMPO 5 SEGNACOLLI ALLA VOLTA
092600920424     C     X             IFGT      10
092700930917     C     *INOF         IFEQ      *ON
092800930917     C                   EXCEPT    TESTA
092900930917     C                   EXCEPT    FIT2
093000930917     C                   SETOFF                                       05
093100930917     C                   SETOFF                                       OF
093200930917     C                   ENDIF
093300920413     C*
093400920413     C* DETTAGLIO
093500920413     C                   EXCEPT    DET2
093600920413     C  N05              SETON                                        05
093700920413     C*
093800920413     C* AZZERO SCHIERA ED INDICATORI
093900920413     C                   MOVEA     *ZEROS        SCN
094000920413     C                   Z-ADD     1             X
094100920413     C                   END
094200920413     C*
094300950104     C     KARB2         READE     FNART000                               32
094400920413     C                   END
094500920413     C*
094600920413     C* STAMPO ULTIMA VOLTA
094700920413     C     X             IFGT      1
094800930917     C     *INOF         IFEQ      *ON
094900930917     C                   EXCEPT    TESTA
095000930917     C                   EXCEPT    FIT2
095100930917     C                   SETOFF                                       OF
095200930917     C                   ENDIF
095300930917     C*
095400920413     C                   EXCEPT    DET2
095500920413     C                   END
095600920413     C*
095700920413     C                   ENDSR
095800920413     C*
095900920413     C*--- STAMPA TOTALI ---------------------------------------------*
096000920316     C*
096100920316     C     STATOT        BEGSR
096200920316     C*
096300920316     C* SE TROVATE SPEDIZIONI STAMPA TOTALI
096400920316     C     NUSPE         IFNE      0
096500920413     C* TOTALE VOLUMI PER LINEA
096600920415     C     VRET          ADD       VPRT          TVOL
096700920415     C                   ADD       VAUT          TVOL             10 2
096800920316     C* SALTO PAGINA
096900920316     C   OF              EXCEPT    TESTA
097000920316     C   OF              SETOFF                                       OF
097100920316     C                   EXCEPT    TOT
097200920316     C* AZZERO I CAMPI DI TOTALE
097300920316     C                   Z-ADD     *ZEROS        NUSPE
097400920316     C                   Z-ADD     *ZEROS        NCOLLI
097500920316     C                   Z-ADD     *ZEROS        TPESO
097600920316     C                   Z-ADD     *ZEROS        O35
097700920316     C                   Z-ADD     *ZEROS        F35
097800920413     C                   Z-ADD     *ZEROS        VRET
097900920413     C                   Z-ADD     *ZEROS        VPRT
098000920413     C                   Z-ADD     *ZEROS        VAUT
098100920316     C                   END
098200920316     C*
098300920415     C     VIDFLG        IFEQ      '1'
098400920415     C                   CLOSE     PRTF198
098500920415     C                   OPEN      PRTF198
098600920415     C                   SETON                                        OF
098700920415     C                   END
098800920415     C*
098900920316     C                   ENDSR
099000950104     C*
099100950104     C*---- DEFINIZIONE CAMPI E KLIST --------------------------------*
099200950105     C     *INZSR        BEGSR
099300950105     C*
099400950104     C                   Z-ADD     1             CODUT
099500950104     C                   CALL      'X§PARUT'
099600950104     C                   PARM                    UT§DSE
099700950104     C                   MOVEL     RAGUT         RSUT             20
099800950104     C                   MOVEL     REC80         CNCR80
099900961127     C* LA DATA DEL GIORNO LA PRENDO DA TIME
100000961127     C                   TIME                    W0140            14 0
100100961127     C                   MOVE      W0140         UDATE8            8 0
100200961127     C                   TIME                    UTIME             6 0
100300961127     C*
100400950104     C     KARB2         KLIST
100500950104     C                   KFLD                    ARBAAS
100600950104     C                   KFLD                    ARBLNP
100700950104     C                   KFLD                    ARBNRS
100800950104     C                   KFLD                    ARBNSP
100900991105     C     KAR6          KLIST
101000991105     C                   KFLD                    ARBAAS
101100991105     C                   KFLD                    ARBLNP
101200991105     C                   KFLD                    ARBNRS
101300991105     C                   KFLD                    ARBNSP
101400991105     C                   KFLD                    KTRC
101500991108     C     KARI          KLIST
101600991108     C                   KFLD                    ARBAAS
101700991108     C                   KFLD                    ARBLNP
101800991108     C                   KFLD                    ARBNRS
101900991108     C                   KFLD                    ARBNSP
102000991108     C                   KFLD                    KTIP
102100950104     C     KTAB          KLIST
102200950104     C                   KFLD                    CODUT
102300950104     C                   KFLD                    COD               2
102400950104     C                   KFLD                    KEY               8
102500950104     C     KTAB2         KLIST
102600950104     C                   KFLD                    CODUT
102700950104     C                   KFLD                    COD
102800950104     C     KARB          KLIST
102900950104     C                   KFLD                    VIDFIL
103000950104     C                   KFLD                    COMAAS            4 0
103100950104     C                   KFLD                    COMMGS            4 0
103200991105     C*
103300991108     C     *LIKE         DEFINE    ARIIPP        WIPP
103400991108     C     *LIKE         DEFINE    ARIIPP        WCONVE
103500991108     C     *LIKE         DEFINE    AR6TRC        KTRC
103600991108     C     *LIKE         DEFINE    AR6TRC        KTIP
103700991108     C                   MOVEL     'A'           KTIP
103800991105     C**
103900991105     C* CARICO DIVISA CORRENTE
104000991105     C**
104100991105     C                   CLEAR                   DSBS02
104200991105     C                   MOVEL     'L'           T02TLA
104300991105     C                   MOVEL     'C'           T02MOD
104400991105     C                   MOVEL     KNSIF         T02SIF
104500991105     C                   MOVEL     'GED'         T02COD
104600991105     C                   MOVEL(P)  1             T02KE1
104700991105     C                   CALL      'TIBS02R'
104800991105     C                   PARM                    KPJBA
104900991105     C                   PARM                    DSBS02
105000991105    2C     T02ERR        IFEQ      ' '
105100991105     C                   MOVEL     T02UNI        DGED
105200991105     C                   ELSE
105300991105     C                   CLEAR                   DGED
105400991105     C                   ENDIF
105500991105     C**
105600991105     C                   MOVEL     'CV'          COD
105700991105     C                   MOVEL     *BLANKS       KEY
105800011025     C                   MOVEL     §GEDCN        KEY
105900991105     C     KTAB          CHAIN     TABEL                              32
106000991105     C  N32              MOVEL     TBLUNI        DSCV
106100991105     C   32              CLEAR                   DSCV
106200991105     C** DIVISA CON DECIMALI
106300991105     C     §CVFDC        IFEQ      'S'
106400991105     C                   SETON                                        95
106500991105     C                   ENDIF
106600950104     C*
106700950104     C                   ENDSR
106800920228     C*
106900961127     C*---------------------------------------------------------------*
107000961230     O*
107100961213     OPRTF198   E            TESTA            02
107200920305     O                       TE(1)              132
107300920306     O                       TE(2)              198
107400900523     O                       RSUT                20
107500961127     O                       UDATE8             185 '  /  /    '
107600920305     O                       PAGE          Z    198
107700961127     O          E            TESTA            03
107800961127     O                       UTIME              183 '  :  :  '
107900920505     O          E            TESTA            04
108000060216     O                                              'LINEA DI ARRIVO'
108100900523     O                       VIDFIL            +  1
108200060216     O                                         +  0 '-'
108300060216     O                       DESFIL            +  0
108400060216     O                  01                       56 'DATA CONSEGNA'
108500060216     O                  01 02                    60 'DAL'
108600060216     O                  01N02                    60 'DEL'
108700060216     O                  01   VIDDBD              71 '  /  /    '
108800060216     O                  01 02                    74 'AL'
108900060216     O                  01 02VIDDBA              85 '  /  /    '
109000991105     O                  08                      107 'DATA SPEDIZIONE'
109100991105     O                  08 28                   111 'DAL'
109200991105     O                  08N28                   111 'DEL'
109300991105     O                  08   VIDDSD             122 '  /  /    '
109400991105     O                  08 28                   125 'AL'
109500991105     O                  08 28VIDDSA             136 '  /  /    '
109600991105     O                  22                      157 'LINEA PARTENZA:'
109700991105     O                  22   VIDLNP             161
109800991105     O                                          182 'TUTTI GLI IMPORTI '
109900991105     O                                          195 'ESPRESSI IN: '
110000011025     O                       §GEDCN             198
110100900523     O          E            TESTA       2
110200930128     O                       TE(22)             198
110300930128     O          E            TESTA       1
110400920305     O                       TE(5)               66
110500920305     O                       TE(6)              132
110600920416     O                       TE(8)              198
110700920414     O          E            TESTA       1
110800920414     O                       TE(7)              132
110900920416     O                       TE(9)              198
111000900523     O          E            TESTA       1
111100920416     O                       TE(10)              66
111200920416     O                       TE(10)             132
111300920416     O                       TE(10)             198
111400920410     O          E            DET         1  0
111500920228     O                       ARBNSP        Z   +  1
111600920228     O                       ARBLNP            +  1
111700900523     O                       STARSM            +  1
111800950104     O                       STARSD            +  1
111900920228     O                       ARBNCL        Z   +  1
112000950105     O                       ARBRMN        Z   +  4
112100920228     O                     23ARBNRS        Z   +  1
112200900523     O                     23                  +  0 '-'
112300920228     O                       ARBNCD        Z   +  0
112400910130     O                                         +  0 '/'
112500920228     O                       ARBNCA        Z   +  0
112600991105     O               95      STDIMV        4    118
112700991105     O              N95      ST0IMV        Z    118
112800991105     O                       VIDDAM            +  2 '  /  '
112900920316     O          E    03      DET         0  1
113000920316     O          E    04      DET         0  1
113100991105     O                       VIDDDC             132 '  /  '
113200991105     O                       STADCP            +  2 '  /  '
113300991105     O                       VIDDCM             146 '  /  '
113400991108     O                     35SK1(B)             152
113500991108     O                     35                   153 '-'
113600920511     O                     35DE1(B)            +  0
113700991108     O                     36ST1SK2            +  2
113800950105     O                     36                  +  0 '-'
113900991108     O                     36ST1DE2            +  0
114000991108     O                       ARBFIP             187
114100991108     O                                          188 '-'
114200991108     O               95      STDIPP        4    198
114300991108     O              N95      ST0IPP        Z    198
114400920415     O          E            DET         0  1
114500900523     O                       VIDDSP                 '  /  /  '
114600900523     O                       STAINM            +  5
114700900523     O                       STAIND            +  1
114800950105     O                       STAPKF        4   +  0
114900950105     O                       ARBCBO            +  1
115000950105     O                                         +  0 '-'
115100950105     O                       STACBL            +  0
115200991105     O                       ARBNAS            +  2
115300991105     O              N95      ST0CAS        Z    118
115400991105     O               95      STDCAS        4    118
115500991105     O                     04ARBNDC        Z    132
115600991108     O               94    04ARBHMC             146 '0 :  '
115700950104     O* Ex lasciato avviso
115800950105     O*                                +  1 '            '
115900950105     O*                                +  0 '     '
116000950104     O*
116100991108     O                     27SK3(D)             149
116200930224     O                     27                  +  0 '-'
116300930224     O                     27DE3(D)            +  0
116400991108     O                     37ST2SK2            +  4
116500950105     O                     37                  +  0 '-'
116600950105     O                     37ST2DE2            +  0
116700950104     O                       STADES             198
116800920316     O          E            DET         0  1
116900920410     O                       ARBKSC        Z   +  1
117000950105     O                       STALOM            +  5
117100920228     O                       ARBPRM            +  1
117200950105     O                       STALOD            +  2
117300920228     O                       ARBPRD            +  1
117400950105     O                       ARBFVF            +  1
117500950105     O                       ARBVLF        4   +  0
117600950105     O                     21                  +  1 'ImpAs'
117700991105     O              N95    21ST0IAS        Z     86
117800991105     O               95    21STDIAS        4     86
117900950105     O                       S94NTC            +  2
118000950105     O                  14 03                   139 'Giacenza:Cod.'
118100930128     O                  14 03ARBCMC            +  1
118200950109     O                     15                   154 'Fattura:'
118300930128     O                     15STANFT        Z   +  1
118400930128     O                     15                  +  1 'del'
118500930128     O                     15STADFT            +  1 '  /  '
118600950109     O                     15                   187 'Tip.Inc:'
118700930128     O                     15ARBMIF            +  0
118800930129     O                  15 16                  +  1 'Nota Acc'
118900930917     O**
119000930129     O          E            RIT         0  0
119100930315     O                                         + 46 'Ritirante  Merce'
119200930315     O                                         + 10 ':'
119300930129     O                       S91NTC            +  2
119400930917     O**
119500020517     O          E            NOTe        0  1
119600950109     O                                          159 'N   O   T   E'
119700950105     O                                         +  1 ':'
119800930129     O                       S92NTC             198
119900930917     O**
120000930129     O          E    25N26   LEGA        0  0
120100930315     O                                         + 46 'Legata a bolla'
120200920424     O                                         +  1 'PRECEDENTE :'
120300920424     O                       LBLAAP            +  2
120400930128     O                       LBLLPP        Z   +  1
120500920424     O                       LBLNRP        Z   +  2
120600920424     O                                         +  0 '-'
120700920424     O                       LBLNSP        Z   +  0
120800930129     O                                         +  2 'del'
120900930128     O                       STADSP            +  1 '  /  /  '
121000930129     O                                         +  2 'L. Arrivo:'
121100930129     O                       LBLLAP            +  1
121200930129     O                                         +  0 '-'
121300930129     O                       DESLNA            +  0
121400930129     O          E    25 26   LEGA        0  0
121500930315     O                                         + 46 'Legata a bolla'
121600930312     O                                         +  1 'SEGUENTE   :'
121700930312     O                       AAN(X)            +  2
121800930312     O                       LPN(X)        Z   +  1
121900930312     O                       NRN(X)        Z   +  2
122000920424     O                                         +  0 '-'
122100930312     O                       NSN(X)        Z   +  0
122200930312     O                                         +  2 'del'
122300930312     O                       DSN(X)            +  1 '  /  /  '
122400930128     O                                         +  2 'L. Arrivo:'
122500930312     O                       LAN(X)            +  1
122600930127     O                                         +  0 '-'
122700930312     O                       DEC(X)            +  0
122800930917     O**
122900930129     O          E            FIT         0  1
123000930129     O                                          198 ' '
123100930316     O          E            FIT2        1  0
123200930316     O                                          198 ' '
123300930917     O**
123400930129     O          E            DET2           1
123500920413     O                    N05                    91 'SEGNAC. NON CONSECUTIVI:'
123600910808     O                       SCN(1)        Z     99
123700910808     O                       SCN(2)        Z    107
123800910808     O                       SCN(3)        Z    115
123900910808     O                       SCN(4)        Z    123
124000910808     O                       SCN(5)        Z    131
124100920424     O                       SCN(6)        Z    139
124200920424     O                       SCN(7)        Z    147
124300920424     O                       SCN(8)        Z    155
124400920424     O                       SCN(9)        Z    163
124500920424     O                       SCN(10)       Z    171
124600930917     O**
124700900523     O          E            TOT         1
124800920416     O                       TE(11)              66
124900920416     O                       TE(12)             132
125000920416     O                       TE(13)             198
125100900523     O          E            TOT         1
125200900523     O                                            1 '*'
125300920305     O                                           35 '*'
125400920305     O                                           66 '*'
125500920413     O                                          132 '*'
125600920413     O                                          198 '*'
125700900523     O          E            TOT         1
125800920416     O                       TE(14)              66
125900920416     O                       TE(15)             132
126000920416     O                       TE(18)             198
126100920306     O                       NUSPE         Z     28
126200920305     O                       NCOLLI        Z     60
126300920306     O                       F35           2    122
126400920413     O                       VRET          2    180
126500900523     O          E            TOT         1
126600920416     O                       TE(16)             132
126700920416     O                       TE(19)             198
126800900523     O                                            1 '*'
126900920306     O                                           35 '*'
127000920306     O                                           66 '*'
127100920306     O                       O35           2    122
127200920413     O                       VPRT          2    180
127300920413     O          E            TOT         1
127400920416     O                       TE(20)             198
127500920413     O                                            1 '*'
127600920413     O                                           35 '*'
127700920413     O                                           66 '*'
127800920413     O                       VAUT          2    180
127900920413     O          E            TOT         1
128000920416     O                       TE(17)             132
128100920416     O                       TE(21)             198
128200920413     O                                            1 '*'
128300920413     O                                           35 '*'
128400920413     O                                           66 '*'
128500920413     O                                          132 '*'
128600920415     O                       TPESO         2    122
128700920413     O                       TVOL          2    180
128800900523     O          E            TOT         1
128900920416     O                       TE(10)              66
129000920416     O                       TE(10)             132
129100920416     O                       TE(10)             198
129200900523     O                                            1 '*'
129300920305     O                                          198 '*'
129400961230     C*---------------------------------------------------------------*
129500991108**
129600961230 ***  E L E N C O    S P E D I Z I O N I    I N    A R R I V O  **   1
129700961127*                              FNLR61R                   PAG. XXXX   2
129800961213                                                                     3
129900961213                                                                     4
130000930128 SPEDIZ. LNP MITTENTE                 DESTINATARIO              C/   5
130100991105P/V        RIFERIMENTI SER   SEGNACOLLI   IMPONIBILE ARRIVO DISTIN   6
130200991105             COD.BOLLA                    C/ASSEGNO  MERCE  DAT/N.   7
130300991108 1°CONS  DATA   AUTOTRASPORTATORE    CONS.            TIPO E IMP.    8
130400991108  PARZ    ORA      CONS.ANOMALA      PARTICOLARI      INC.PARZIALE   9
130500930128------------------------------------------------------------------  10
130600930128*-----------------------------------------------------------------  11
130700930128----------------- T O T A L I    G E N E R A L I -----------------  12
130800930128-----------------------------------------------------------------*  13
130900930128* SPEDIZIONI :       XXXXXXX      *    COLLI :       XXXXXXX     *  14
131000930128           PESO : FINO A 35 QUINTALI          XXXXXXXXXX         *  15
131100930128                  OLTRE  35 QUINTALI          XXXXXXXXXX         *  16
131200930128                  TOTALE PESO KG.             XXXXXXXXXX         *  17
131300930128              VOLUME :  REALE           XXXXXXXX                 *  18
131400930128                        PRES.           XXXXXXXX                 *  19
131500930128                        AUTOM.          XXXXXXXX                 *  20
131600930128                        TOTALE        XXXXXXXXXX                 *  21
131700930128     C       O       N       S       E       G       N       A      22
131800991105                                         XXXXXXXXXX  XX/XX  XX/XX
131900991108 XX/XX  XX/XX  XXXX-XXXXXXXXXXXXXX  X-XXXXXXX         X-XXXXXXXXXX
