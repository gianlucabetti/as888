000100000000     H*------------------------------------------------------------------------*
000200001016     H* CONTROLLO ASSORBIMENTO
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000914     H*------------------------------------------------------------------------*
000600000000     FAZORG01L  IF   E           K DISK
000700001016     FTABEL00F  IF   E           K DISK
000800001016     FTITAS38C  IF   E           K DISK
000900001016     FTITAA30C  IF   E           K DISK
001000001016     FWFASS01L  UF A E           K DISK    USROPN
001100001113     FWFASS02L  IF   E           K DISK    USROPN
001200001113     F                                     RENAME(WFASS000:WFASSSTP)
001300001109     FTIST45P   O    E             PRINTER OFLIND(*IN01)
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000914     D* FILIALI POSTE
001900000000     D*-------------------
002000000915     D FIL             S              3  0 DIM(500)                             *FILIALE
002100001106     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
002200000915     D ARE             S              3  0 DIM(500)                             *AREA
002300000915     D DIV             S              1    DIM(500)                             *DIVISIONE
002400020226     D NTX             S              3    DIM(500)                             *NETWORK
002500000920     D*------------
002600000920     D* COMANDI
002700000920     D*------------
002800000920     D CMD             S             80    DIM(2) CTDATA PERRCD(1)
002900000000     D*-------------------
003000000918     D* SCHIERE FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA E TEMPI DI CONSEGNA
003100000914     D*-------------------
003200001025     D RBL             S              1    DIM(20)                              RAGGRUPPAMENTO BOLLA
003300001025     D FSP             S              1    DIM(20)                              FLAG SPEDIZIONE
003400001025     D FRI             S              1    DIM(20)                              FLAG RICAVO
003500000000     D*-------------------
003600000918     D* SCHIERE TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003700000000     D*-------------------
003800000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
003900000914     D SP              S              1    DIM(20)                              FLAG SPEDIZIONE
004000000914     D RI              S              1    DIM(20)                              FLAG RICAVO
004100020226     D*-------------------
004200020226     D* SCHIERA FILIALI FEDEX
004300020226     D*-------------------
004400020226     D FILFED          S              3  0 DIM(999)
004500020226     D*-------------------
004600020226     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
004700020226     D*-------------------
004800020226     D OG143         E DS
004900000000     D*------------------------------------------------------------------------*
005000000000     D* INPUT
005100000000     D*------------------------------------------------------------------------*
005200000000     D*-------------------
005300000000     D* ARCHITETTURA
005400000000     D*-------------------
005500000000     D KPJBA         E DS
005600000000     D*-------------------
005700000000     D* DS TIPO BOLLA
005800000000     D*-------------------
005900000000     D DSTB          E DS
006000000000     D*-------------------
006100000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
006200000000     D*-------------------
006300000000     D DS2W          E DS
006400001016     D*------------------
006500001016     D* DS REPERIMENTO DATI UTENTE
006600001016     D*-------------------
006700001016     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
006800001016     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
006900001016     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
007000001016     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
007100001016     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
007200000000     D*-------------------
007300001016     D* DS SCOMPOSIZIONE DATE INIZ/FIN/WRK
007400000000     D*-------------------
007500000000     D                 DS                  INZ
007600000000     D  DIANN                  1      4  0
007700000000     D  DIMGS                  5      8  0
007800000914     D  DIDAT                  1      8  0
007900000914     D                 DS                  INZ
008000000914     D  DFANN                  1      4  0
008100000914     D  DFMGS                  5      8  0
008200000914     D  DFDAT                  1      8  0
008300001016     D                 DS                  INZ
008400001016     D  DWANN                  1      4  0
008500001016     D  DWMGS                  5      8  0
008600001016     D  DWDAT                  1      8  0
008700000000     D*-------------------
008800000000     D* DS PARAMETRI DI LANCIO PGM
008900000000     D*-------------------
009000020226     D PARAM           DS
009100020226     D  PARDAI                 1      8  0
009200020226     D  PARDAF                 9     16  0
009300020226     D  PARKG1                17     19  0
009400020226     D  PARKG2                20     22  0
009500020226     D  PARKG3                23     25  0
009600020226     D  PARKG4                26     28  0
009700020226     D  PARKG5                29     31  0
009800020226     D  PARMC1                32     34  2
009900020226     D  PARMC2                35     37  2
010000020226     D  PARMC3                38     40  2
010100020226     D  PARMC4                41     43  2
010200020226     D  PARMC5                44     46  2
010300020226     D  PARNAZ                47     49
010400020226     D  PARCAD                50     51
010500020226     D  PARCAA                52     53
010600020226     D  PARTP1                54     56  0
010700020226     D  PARTP2                57     59  0
010800020226     D  PARTP3                60     62  0
010900020226     D  PARTP4                63     65  0
011000020226     D  PARTP5                66     68  0
011100020226     D  PARTP6                69     71  0
011200020226     D  PARTP7                72     74  0
011300020226     D  PARTP8                75     77  0
011400020226     D  PARTP9                78     80  0
011500020226     D  PARTP0                81     83  0
011600020226     D  PARNT1                84     86
011700020226     D  PARNT2                87     89
011800020226     D  PARNT3                90     92
011900020226     D  PARNT4                94     96
012000020226     D  PARNT5                97     99
012100020226     D  PARNT6               100    102
012200020226     D  PARGEN               103    103
012300020226     D  PARDET               104    104
012400020226     D  PARGIO               105    107  0
012500020226     D  PARIAD               108    115  0
012600020226     D  PARFAD               116    123  0
012700020226     D  PARDRI               124    131  0
012800001016     D*-------------------
012900001016     D* LETTURA CAMPO ORGDE£ DI AZORG
013000001016     D*-------------------
013100001016     D DSSI95        E DS                  EXTNAME(tisi95ds)                    *ds "TISI95R"
013200001109     D*-------------------
013300001109     D* DS DI WRK PER REDEFINIZIONE TERMINAL ALFA
013400001109     D*-------------------
013500001114     D  ALFTP1                 1      3    inz(*blanks)
013600001109     D  FIX1                   4      4    inz('/')
013700001114     D  ALFTP2                 5      7    inz(*blanks)
013800001114     D  FIX2                   8      8    inz('/')
013900001114     D  ALFTP3                 9     11    inz(*blanks)
014000001114     D  FIX3                  12     12    inz('/')
014100001114     D  ALFTP4                13     15    inz(*blanks)
014200001114     D  FIX4                  16     16    inz('/')
014300001114     D  ALFTP5                17     19    inz(*blanks)
014400001114     D  FIX5                  20     20    inz('/')
014500001114     D  ALFTP6                21     23    inz(*blanks)
014600001114     D  FIX6                  24     24    inz('/')
014700001114     D  ALFTP7                25     27    inz(*blanks)
014800001114     D  FIX7                  28     28    inz('/')
014900001114     D  ALFTP8                29     31    inz(*blanks)
015000001114     D  FIX8                  32     32    inz('/')
015100001114     D  ALFTP9                33     35    inz(*blanks)
015200001114     D  FIX9                  36     36    inz('/')
015300001114     D  ALFTP0                37     39    inz(*blanks)
015400001114     D ALFATER                 1     39
015500000914     D*------------------------------------------------------------------------*
015600000918     D* VARIABILI DI WRK
015700000914     D*------------------------------------------------------------------------*
015800000914     D i               s              5i 0 inz(*zeros)
015900000915     D y               s              5i 0 inz(*zeros)
016000000914     D j               s              5i 0 inz(*zeros)
016100001016     D wrkcap          s              2    inz(*blanks)
016200001016     D chkind          s              1    inz(*blanks)
016300001109     D wrecok          s              1
016400001109     D depcli          S              4  0 inz(*zeros)
016500001120     D depdat          S              8  0 inz(*zeros)
016600001016     D*---
016700001016     D* Variabili riferite al data base
016800001016     D*---
016900001016     D ktaaas          S                   LIKE(taaaas)                         *lett. titas38c
017000001016     D ktamgs          S                   LIKE(tasmgs)
017100001016     D ktaksc          S                   LIKE(tasksc)
017200001016     D kaAaas          S                   LIKE(taaaas)                         *lett. titaA30c
017300001016     D kaAlnp          S                   LIKE(taalnp)
017400001016     D kaAnrs          S                   LIKE(taanrs)
017500001016     D kaAnsp          S                   LIKE(taansp)
017600001016     D kaAtrc          S                   LIKE(taatrc)
017700001016     D ktbute          S                   LIKE(tblkut)                         *lettura tabel00f
017800001016     D ktbcod          S                   LIKE(tblcod)
017900001016     D ktbkey          S                   LIKE(tblkey)
018000001016     D korfil          S                   LIKE(orgfil)
018100001113     D kwacdi          S                   LIKE(asscdi)
018200001113     D kwalna          S                   LIKE(asslna)
018300001113     D kwalnp          S                   LIKE(asslnp)
018400001113     D kwacar          S                   LIKE(asscar)
018500001113     D kwacla          S                   LIKE(asscla)
018600001113     D kwaczc          S                   LIKE(assczc)
018700001016     D kwaver          S                   LIKE(assver)
018800001016     D kwalnx          S                   LIKE(asslnx)
018900001016     D kwanaz          S                   LIKE(assnaz)
019000001016     D kwaprv          S                   LIKE(assprv)
019100001016     D kwaloc          S                   LIKE(assloc)
019200001030     D kwacap          S                   LIKE(asscap)
019300001110     D kwantw          S                   LIKE(assntw)
019400001113     D depcla          S                   LIKE(asscla)
019500001113     D depczc          S                   LIKE(assczc)
019600001106     D depdiv          S                   LIKE(assdiv)
019700001106     D depare          S                   LIKE(assare)
019800001106     D depfil          S                   LIKE(asslnx)
019900001106     D depprv          S                   LIKE(assprv)
020000001106     D depcap          S                   LIKE(asscap)
020100001106     D deploc          S                   LIKE(assloc)
020200001113     D depnaz          S                   LIKE(assnaz)
020300001113     D depntw          S                   LIKE(assntw)
020400000914     D*-------------------
020500000914     D* DS TEMPI DI CONSEGNA
020600000914     D*-------------------
020700000914     D DJPT          E DS
020800000918     D*-------------------
020900000918     D* DS RAGGRUPPAMENTI SERIE CLIENTI POSTE
021000000918     D*-------------------
021100000918     D DRSR          E DS
021200000921     D*------------------
021300000921     D* DS "XSRDA8" - CONTROLLA DATA (8)
021400000921     D*------------------
021500000921     D WLBDA8          DS                  INZ
021600000921     D  G08DAT                 1      8  0
021700000921     D  G08INV                 9     16  0
021800000921     D  G08ERR                17     17
021900000921     D  G08TGI                18     22  0
022000000000     C*------------------------------------------------------------------------*
022100000000     C* MAIN LINE
022200000000     C*------------------------------------------------------------------------*
022300000000     C*
022400000920     C                   exsr      procedi
022500001106     C                   exsr      stampa
022600001109     C*
022700001109     C* Operazioni di chiusura
022800001109     C                   EXSR      chiude
022900000914     C*
023000000914     C                   SETON                                        LR
023100000914     C*------------------------------------------------------------------------*
023200000914     C* PROCEDI - Routine di elaborazione
023300000914     C*------------------------------------------------------------------------*
023400000914     C     PROCEDI       BEGSR
023500001106     C*
023600001106     C* Se richiesto nel lancio a video ==> CLRPFM del file di wrk (WFASS00F)
023700001106     C                   IF        PARGEN = 'S'
023800001016     C                   Z-ADD     80            LENGH            15 5
023900001026     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024000000920     C                   PARM                    CMD(1)
024100000920     C                   PARM                    LENGH
024200000920     C*
024300000920     C* Se riuscito comando ==> procedo
024400001106     C                   if        not %error
024500000914     C*
024600001016     C* Apro il wrk-file (WFASS01L)
024700001016     C                   open      wfass01l
024800000914     C*
024900000914     C* Scompongo i parametri ricevuti in input
025000000914     C                   z-add     pardai        didat
025100000914     C                   z-add     pardaf        dfdat
025200001016     C*
025300001016     C* Posizionemento e prima lettura
025400001016     C                   z-add     diann         ktaaas
025500001016     C                   z-add     dimgs         ktamgs
025600001016     C                   z-add     *zeros        ktaksc
025700001016     C*
025800001016     C     keytas        setll     titas38c                           99        *FINE FILE
025900001016     C*
026000001016     C                   if        %found(titas38c)
026100001016     C                   read(e)   titas38c
026200001016     C                   z-add     tasaas        dwann
026300001016     C                   z-add     tasmgs        dwmgs
026400001016     C                   dow       not %eof(titas38c) and
026500001016     C                             dwdat <= dfdat
026600001016     C                   exsr      decodi
026700001016     C*
026800001016     C                   if        errl00 = '0'
026900001016     C                   exsr      creap
027000001016     C                   exsr      creaa
027100001016     C                   endif
027200001016     C*
027300001016     C                   read(e)   titas38c
027400001016     C                   z-add     tasaas        dwann
027500001016     C                   z-add     tasmgs        dwmgs
027600001016     C                   enddo
027700001016     C                   endif
027800000914     C*
027900001016     C* Chiudo il wrk-file (WFASS01L)
028000001016     C                   close     wfass01l
028100000920     C                   endif
028200001106     C*
028300001106     C                   ENDIF
028400000920     C*
028500000914     C                   ENDSR
028600001106     C*------------------------------------------------------------------------*
028700001106     C* STAMPA - Routine di stampa
028800001106     C*------------------------------------------------------------------------*
028900001106     C     STAMPA        BEGSR
029000001207     C*
029100001207     C* Inizializzo la testata
029200001207     C                   clear                   ST45TE
029300001109     C*
029400001109     C* Imposto i parametri in testata
029500001109     C                   z-add     PARKG1        STPPP1A
029600001109     C                   z-add     PARKG1        STPPP1D
029700001109     C                   z-add     PARKG2        STPPP2A
029800001109     C                   z-add     PARKG2        STPPP2D
029900001109     C                   z-add     PARKG3        STPPP3A
030000001109     C                   z-add     PARKG3        STPPP3D
030100001109     C                   z-add     PARKG4        STPPP4A
030200001109     C                   z-add     PARMC1        STPPV1A
030300001109     C                   z-add     PARMC1        STPPV1D
030400001109     C                   z-add     PARMC2        STPPV2A
030500001109     C                   z-add     PARMC2        STPPV2D
030600001109     C                   z-add     PARMC3        STPPV3A
030700001109     C                   z-add     PARMC3        STPPV3D
030800001109     C                   z-add     PARMC4        STPPV4A
030900001109     C                   z-add     PARIAD        STPDAI
031000001109     C                   z-add     PARFAD        STPDAF
031100001120     C                   z-add     depdat        STPDRI                         *DATA RIF. G/M/AAAA
031200001120     C*
031300001109     C                   z-add     PARGIO        STPGIO
031400001109     C                   if        PARNAZ = *blanks
031500001109     C                   movel     'ITA'         STPNAZ
031600001109     C                   else
031700001109     C                   movel     PARNAZ        STPNAZ
031800001110     C                   endif
031900020227     C                   if        PARNT1 <> *blanks
032000020226     C                   eval      STPNTW = %trim(STPNTW) + %trim(PARNT1) + '/'
032100020227     C                   endif
032200020227     C                   if        PARNT2 <> *blanks
032300020226     C                   eval      STPNTW = %trim(STPNTW) + %trim(PARNT2) + '/'
032400020227     C                   endif
032500020227     C                   if        PARNT3 <> *blanks
032600020226     C                   eval      STPNTW = %trim(STPNTW) + %trim(PARNT3) + '/'
032700020227     C                   endif
032800020227     C                   if        PARNT4 <> *blanks
032900020226     C                   eval      STPNTW = %trim(STPNTW) + %trim(PARNT4) + '/'
033000020227     C                   endif
033100020227     C                   if        PARNT5 <> *blanks
033200020226     C                   eval      STPNTW = %trim(STPNTW) + %trim(PARNT5) + '/'
033300020227     C                   endif
033400020227     C                   if        PARNT6 <> *blanks
033500020226     C                   eval      STPNTW = %trim(STPNTW) + %trim(PARNT6) + '/'
033600020227     C                   endif
033700020312     C* Se nn specificati network nel lancio evidenzio "TUTTI"
033800020312     C                   if        STPNTW = *blanks
033900020312     C                   eval      STPNTW = 'TUTTI I NETWORK'
034000020312     C                   endif
034100020312     C*
034200001109     C                   if        PARCAD = *blanks
034300001109     C                   movel     *zeros        STPCAD
034400001109     C                   else
034500001109     C                   movel     PARCAD        STPCAD
034600001109     C                   endif
034700001109     C                   if        PARCAA = *blanks
034800001109     C                   movel     *all'9'       STPCAA
034900001109     C                   else
035000001109     C                   movel     PARCAA        STPCAA
035100001109     C                   endif
035200001109     C                   eval      ALFTP1 = %editc(PARTP1:'Z')
035300001109     C                   eval      ALFTP2 = %editc(PARTP2:'Z')
035400001109     C                   eval      ALFTP3 = %editc(PARTP3:'Z')
035500001109     C                   eval      ALFTP4 = %editc(PARTP4:'Z')
035600001109     C                   eval      ALFTP5 = %editc(PARTP5:'Z')
035700001109     C                   eval      ALFTP6 = %editc(PARTP6:'Z')
035800001109     C                   eval      ALFTP7 = %editc(PARTP7:'Z')
035900001109     C                   eval      ALFTP8 = %editc(PARTP8:'Z')
036000001109     C                   eval      ALFTP9 = %editc(PARTP9:'Z')
036100001109     C                   eval      ALFTP0 = %editc(PARTP0:'Z')
036200001114     C                   movel(p)  ALFATER       STPTER
036300001106     C*
036400001113     C* Apro il wrk-file (WFASS02L)
036500001113     C                   open      wfass02l
036600001106     C*
036700001106     C* Posizionamento sul file di wrk e prima lettura
036800001106     C                   exsr      setfil
036900001106     C*
037000001106     C* Ciclo fino a fine file di wrk
037100001106     C     finl00        doweq     '0'
037200001106     C*
037300001106     C* Operazioni per nuova DIVISIONE
037400001106     C                   exsr      newdiv
037500001106     C*
037600001106     C* Ciclo fino a rottura divisione
037700001113     C     finl00        doweq     '0'
037800001113     C     asscdi        andeq     depdiv
037900001106     C*
038000001106     C* Operazioni per nuova AREA
038100001106     C                   exsr      neware
038200001106     C*
038300001106     C* Ciclo fino a rottura di AREA
038400001106     C     finl00        doweq     '0'
038500001113     C     asscdi        andeq     depdiv
038600001113     C     asscar        andeq     depare
038700001106     C*
038800001106     C* Operazioni per nuova FILIALE
038900001106     C                   exsr      newfil
039000001106     C*
039100001106     C* Ciclo fino a rottura di FILIALE
039200001106     C     finl00        doweq     '0'
039300001113     C     asscdi        andeq     depdiv
039400001113     C     asscar        andeq     depare
039500001113     C     asscla        andeq     depfil
039600001113     C*
039700001113     C* Operazioni per buovo PRV/CAP
039800001113     C                   exsr      newcap
039900001113     C*
040000001113     C* Ciclo fino a rottura di PRV/CAP
040100001113     C     finl00        doweq     '0'
040200001113     C     asscdi        andeq     depdiv
040300001113     C     asscar        andeq     depare
040400001113     C     asscla        andeq     depfil
040500001113     C     assnaz        andeq     depnaz
040600001113     C     assprv        andeq     depprv
040700001113     C     asscap        andeq     depcap
040800001113     C*
040900001113     C* Operazioni per buovo PRV/CAP/LOC
041000001113     C                   exsr      newdet
041100001106     C*
041200001113     C* Ciclo fino a rottura di PRV/CAP/LOC
041300001106     C     finl00        doweq     '0'
041400001113     C     asscdi        andeq     depdiv
041500001113     C     asscar        andeq     depare
041600001113     C     asscla        andeq     depfil
041700001113     C     assnaz        andeq     depnaz
041800001106     C     assprv        andeq     depprv
041900001113     C     asscap        andeq     depcap
042000001106     C     assloc        andeq     deploc
042100001106     C*
042200001106     C* Memorizza i dati di DETTAGLIO
042300001106     C     WRECOK        ifeq      'S'
042400001106     C                   exsr      memspe
042500001113     C                   endif
042600001113     C*
042700001113     C* Legge il record successivo
042800001113     C                   exsr      letfil
042900001113     C*
043000001113     C                   enddo
043100001106     C*
043200001106     C* Stampa i dati di DETTAGLIO
043300001106     C     PARDET        ifeq      'S'
043400001113     C                   exsr      stadet
043500001110     C                   endif
043600001113     C*
043700001113     C                   enddo
043800001113     C*
043900001113     C* Stampa il totale CAP
044000001113     C                   exsr      stacap
044100001106     C*
044200001106     C                   enddo
044300001106     C*
044400001106     C* Stampa il totale FILIALE
044500001106     C                   exsr      stafil
044600001106     C*
044700001106     C                   enddo
044800001106     C*
044900001106     C* Stampa il totale AREA
045000001106     C                   exsr      staare
045100001106     C*
045200001106     C                   enddo
045300001106     C*
045400001106     C* Stampa il totale DIVISIONE
045500001106     C                   exsr      stadiv
045600001106     C*
045700001106     C                   enddo
045800001106     C*
045900001106     C* Stampa il totale GENERALE
046000001109     C                   exsr      stagen
046100001106     C*
046200001113     C* Chiudo il wrk-file (WFASS02L)
046300001113     C                   close     wfass02l
046400001106     C*
046500001106     C                   ENDSR
046600001106     C*------------------------------------------------------------------------*
046700001106     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
046800001106     C*------------------------------------------------------------------------*
046900001106     C     SETFIL        BEGSR
047000001106     C*
047100001106     C* Posizionamento
047200001113     C                   movel     'A'           kwaver
047300001113     C                   movel     *blanks       kwacdi
047400001113     C                   z-add     *zeros        kwacar
047500001113     C                   z-add     *zeros        kwacla
047600001113     C                   movel     *blanks       kwanaz
047700001113     C                   movel     *blanks       kwaprv
047800001113     C                   movel     *blanks       kwacap
047900001113     C                   movel     *blanks       kwaloc
048000001113     C                   z-add     *zeros        kwaczc
048100001113     C*
048200001113     C     keywf2        setll     wfass02l                           99
048300001106     C*
048400001106     C     *in99         ifeq      '1'
048500001106     C                   movel     '1'           finl00
048600001106     C                   else
048700001106     C                   exsr      letfil
048800001106     C                   endif
048900001106     C*
049000001106     C                   ENDSR
049100001106     C*------------------------------------------------------------------------*
049200001106     C* LETFIL - LETTURA PROSSIMO RECORD
049300001106     C*------------------------------------------------------------------------*
049400001106     C     LETFIL        BEGSR
049500001106     C*
049600001106     C* LEGGO FINO A:
049700001106     C*     - FINE FILE O SUPERATO LIMITE FINALE
049800001106     C*     - TROVATO RECORD VALIDO
049900001109     C                   movel     'N'           WRECOK
050000001106     C*
050100001106     C* Letture successive alla prima
050200001113     C                   read      wfass02l                               99
050300001106     C*
050400001106     C     *in99         ifeq      '1'                                          *FINE FILE
050500001113     C     assver        orne      'A'
050600001106     C                   movel     '1'           finl00
050700001106     C                   else
050800001106     C                   exsr      chkrec                                       *CONTR.VALIDITA REC.
050900001106     C                   endif
051000001106     C*
051100001106     C                   ENDSR
051200001106     C*------------------------------------------------------------------------*
051300001114     C* CHKREC - Routine di controllo validità record in stampa
051400001106     C*------------------------------------------------------------------------*
051500001106     C     CHKREC        BEGSR
051600001106     C*
051700001106     C                   movel     'S'           WRECOK
051800001106     C*--------------------
051900001106     C* CONTROLLO NETWORK RICHIESTI
052000001114 XXX C*--------------------
052100001114 ==> C* INIZIO PEZZA MOMENTANEA
052200001120     C*                  if        WRECOK = 'S'
052300001120     C*                  if        PARNT1 = *blanks and
052400001120     C*                            PARNT2 = *blanks and
052500001120     C*                            PARNT3 = *blanks and
052600001120     C*                            PARNT4 = *blanks and
052700001120     C*                            PARNT5 = *blanks and
052800001120     C*                            PARNT6 = *blanks
052900001120     C*                  else
053000001120     C*                  if        PARNT1 = 'P' or
053100001120     C*                            PARNT2 = 'P' or
053200001120     C*                            PARNT3 = 'P' or
053300001120     C*                            PARNT4 = 'P' or
053400001120     C*                            PARNT5 = 'P' or
053500001120     C*                            PARNT6 = 'P'
053600001120     C*                  if        asslnp = 180
053700001120     C*                  else
053800001120     C*                  movel     'N'           WRECOK
053900001120     C*                  endif
054000001120     C*                  else
054100001120     C*                  if        asslnp <> 180
054200001120     C*                  else
054300001120     C*                  movel     'N'           WRECOK
054400001120     C*                  endif
054500001120     C*                  endif
054600001120     C*                  endif
054700001120     C*                  endif
054800001114 ==> C* FINE PEZZA MOMENTANEA
054900001120     C                   if        WRECOK = 'S'
055000001120     C                   if        PARNT1 = *blanks and
055100001120     C                             PARNT2 = *blanks and
055200001120     C                             PARNT3 = *blanks and
055300001120     C                             PARNT4 = *blanks and
055400001120     C                             PARNT5 = *blanks and
055500001120     C                             PARNT6 = *blanks
055600001120     C                   else
055700001120     C                   if        assntw = PARNT1 or
055800001120     C                             assntw = PARNT2 or
055900001120     C                             assntw = PARNT3 or
056000001120     C                             assntw = PARNT4 or
056100001120     C                             assntw = PARNT5 or
056200001120     C                             assntw = PARNT6
056300001120     C                   else
056400001120     C                   movel     'N'           WRECOK
056500001120     C                   endif
056600001120     C                   endif
056700001120     C                   endif
056800001114     C*--------------------
056900001106     C* CONTROLLO TERMINAL PARTENZA / ARRIVO
057000001106     C*--------------------
057100001106     C                   if        WRECOK = 'S'
057200001106     C                   if        PARTP1 > *zeros or
057300001106     C                             PARTP2 > *zeros or
057400001106     C                             PARTP3 > *zeros or
057500001106     C                             PARTP4 > *zeros or
057600001106     C                             PARTP5 > *zeros or
057700001106     C                             PARTP6 > *zeros or
057800001106     C                             PARTP7 > *zeros or
057900001106     C                             PARTP8 > *zeros or
058000001106     C                             PARTP9 > *zeros or
058100001106     C                             PARTP0 > *zeros
058200001106     C                   if        asster = PARTP1 or
058300001106     C                             asster = PARTP2 or
058400001106     C                             asster = PARTP3 or
058500001106     C                             asster = PARTP4 or
058600001106     C                             asster = PARTP5 or
058700001106     C                             asster = PARTP6 or
058800001106     C                             asster = PARTP7 or
058900001106     C                             asster = PARTP8 or
059000001106     C                             asster = PARTP9 or
059100001106     C                             asster = PARTP0
059200001106     C                   else
059300001106     C                   movel     'N'           WRECOK
059400001106     C                   endif
059500001106     C                   endif
059600001106     C                   endif
059700001106     C*--------------------
059800001106     C* CONTROLLO NAZIONE PARTENZA/ARRIVO E CAP PARTENZA/ARRIVO
059900001106     C*--------------------
060000001106     C                   if        WRECOK = 'S'
060100001110     C                   if        PARNAZ <> '999'
060200001110     C                   if        assnaz = PARNAZ
060300001110     C                   else
060400001110     C                   movel     'N'           WRECOK
060500001110     C                   endif
060600001110     C                   endif
060700001109     C                   if        PARCAD > *blanks
060800001109     C                   if        %subst(asscap:1:2) >= PARCAD
060900001106     C                   else
061000001106     C                   movel     'N'           WRECOK
061100001106     C                   endif
061200001106     C                   endif
061300001109     C                   if        PARCAA > *blanks
061400001109     C                   if        %subst(asscap:1:2) <= PARCAA
061500001109     C                   else
061600001109     C                   movel     'N'           WRECOK
061700001109     C                   endif
061800001109     C                   endif
061900001106     C                   endif
062000001106     C*
062100001106     C                   ENDSR
062200001106     C*------------------------------------------------------------------------*
062300001106     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
062400001106     C*------------------------------------------------------------------------*
062500001106     C     NEWDIV        BEGSR
062600001106     C*
062700001106     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
062800001113     C                   MOVEL     asscdi        depdiv
062900001106     C*
063000001106     C* DECODIFICA LA DIVISIONE
063100001106     C                   EXSR      DECDIV
063200001110     C*
063300001110     C* DESCRIZIONE IN TESTATA
063400001110     C                   EVAL      STPDIV='DIV  '+depdiv+' '+desdiv
063500001106     C*
063600001106     C* AZZERA I TOTALI DELLA DIVISIONE
063700001106     C                   Z-ADD     *ZEROS        TDFP1            11 1
063800001106     C                   Z-ADD     *ZEROS        TDFP2            11 1
063900001106     C                   Z-ADD     *ZEROS        TDFP3            11 1
064000001106     C                   Z-ADD     *ZEROS        TDFP4            11 1
064100001106     C                   Z-ADD     *ZEROS        TDFV1            13 3
064200001106     C                   Z-ADD     *ZEROS        TDFV2            13 3
064300001106     C                   Z-ADD     *ZEROS        TDFV3            13 3
064400001106     C                   Z-ADD     *ZEROS        TDFV4            13 3
064500001106     C                   Z-ADD     *ZEROS        TDFC1            11 0
064600001106     C                   Z-ADD     *ZEROS        TDFC2            11 0
064700001106     C                   Z-ADD     *ZEROS        TDFC3            11 0
064800001106     C                   Z-ADD     *ZEROS        TDFC4            11 0
064900001106     C                   Z-ADD     *ZEROS        TDFS1            11 0
065000001106     C                   Z-ADD     *ZEROS        TDFS2            11 0
065100001106     C                   Z-ADD     *ZEROS        TDFS3            11 0
065200001106     C                   Z-ADD     *ZEROS        TDFS4            11 0
065300001109     C                   Z-ADD     *ZEROS        TDTOS            11 0
065400001109     C                   Z-ADD     *ZEROS        TDTOC            11 0
065500001109     C                   Z-ADD     *ZEROS        TDTOP            11 1
065600001109     C                   Z-ADD     *ZEROS        TDTOV            13 3
065700001106     C*
065800001106     C                   ENDSR
065900001106     C*------------------------------------------------------------------------*
066000001106     C* NEWARE - OPERAZIONI PER NUOVA AREA
066100001106     C*------------------------------------------------------------------------*
066200001106     C     NEWARE        BEGSR
066300001106     C*
066400001106     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
066500001113     C                   Z-ADD     asscar        depare
066600001106     C*
066700001106     C* DECODIFICA L'AREA
066800001106     C                   EXSR      DECARE
066900001110     C*
067000001110     C* DESCRIZIONE IN TESTATA
067100001110     C                   EVAL      STPARE='AREA '+%editc(depare:'Z')+
067200001110     C                                    '  '+desare
067300001106     C*
067400001106     C* AZZERA I TOTALI DELL'AREA
067500001106     C                   Z-ADD     *ZEROS        TAFP1            11 1
067600001106     C                   Z-ADD     *ZEROS        TAFP2            11 1
067700001106     C                   Z-ADD     *ZEROS        TAFP3            11 1
067800001106     C                   Z-ADD     *ZEROS        TAFP4            11 1
067900001106     C                   Z-ADD     *ZEROS        TAFV1            13 3
068000001106     C                   Z-ADD     *ZEROS        TAFV2            13 3
068100001106     C                   Z-ADD     *ZEROS        TAFV3            13 3
068200001106     C                   Z-ADD     *ZEROS        TAFV4            13 3
068300001106     C                   Z-ADD     *ZEROS        TAFC1            11 0
068400001106     C                   Z-ADD     *ZEROS        TAFC2            11 0
068500001106     C                   Z-ADD     *ZEROS        TAFC3            11 0
068600001106     C                   Z-ADD     *ZEROS        TAFC4            11 0
068700001106     C                   Z-ADD     *ZEROS        TAFS1            11 0
068800001106     C                   Z-ADD     *ZEROS        TAFS2            11 0
068900001106     C                   Z-ADD     *ZEROS        TAFS3            11 0
069000001106     C                   Z-ADD     *ZEROS        TAFS4            11 0
069100001109     C                   Z-ADD     *ZEROS        TATOS            11 0
069200001109     C                   Z-ADD     *ZEROS        TATOC            11 0
069300001109     C                   Z-ADD     *ZEROS        TATOP            11 1
069400001109     C                   Z-ADD     *ZEROS        TATOV            13 3
069500001106     C*
069600001106     C                   ENDSR
069700001106     C*------------------------------------------------------------------------*
069800001106     C* NEWFIL - OPERAZIONI PER NUOVA FILIALE
069900001106     C*------------------------------------------------------------------------*
070000001106     C     NEWFIL        BEGSR
070100001106     C*
070200001106     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
070300001113     C                   Z-ADD     asscla        depfil
070400001106     C*
070500001106     C* DECODIFICA LA FILIALE
070600001106     C                   EXSR      DECFIL
070700001106     C*
070800001106     C* AZZERA I TOTALI DELLA FILIALE
070900001106     C                   Z-ADD     *ZEROS        TFFP1            11 1
071000001106     C                   Z-ADD     *ZEROS        TFFP2            11 1
071100001106     C                   Z-ADD     *ZEROS        TFFP3            11 1
071200001106     C                   Z-ADD     *ZEROS        TFFP4            11 1
071300001106     C                   Z-ADD     *ZEROS        TFFV1            13 3
071400001106     C                   Z-ADD     *ZEROS        TFFV2            13 3
071500001106     C                   Z-ADD     *ZEROS        TFFV3            13 3
071600001106     C                   Z-ADD     *ZEROS        TFFV4            13 3
071700001106     C                   Z-ADD     *ZEROS        TFFC1            11 0
071800001106     C                   Z-ADD     *ZEROS        TFFC2            11 0
071900001106     C                   Z-ADD     *ZEROS        TFFC3            11 0
072000001106     C                   Z-ADD     *ZEROS        TFFC4            11 0
072100001106     C                   Z-ADD     *ZEROS        TFFS1            11 0
072200001106     C                   Z-ADD     *ZEROS        TFFS2            11 0
072300001106     C                   Z-ADD     *ZEROS        TFFS3            11 0
072400001106     C                   Z-ADD     *ZEROS        TFFS4            11 0
072500001109     C                   Z-ADD     *ZEROS        TFTOS            11 0
072600001109     C                   Z-ADD     *ZEROS        TFTOC            11 0
072700001109     C                   Z-ADD     *ZEROS        TFTOP            11 1
072800001109     C                   Z-ADD     *ZEROS        TFTOV            13 3
072900001106     C*
073000001106     C                   ENDSR
073100001113     C*------------------------------------------------------------------------*
073200001113     C* NEWCAP - OPERAZIONI PER NUOVO PRV/CAP
073300001113     C*------------------------------------------------------------------------*
073400001113     C     NEWCAP        BEGSR
073500001113     C*
073600001113     C* MEMORIZZA NUOVO DETTAGLIO
073700001113     C                   movel     assnaz        depnaz
073800001113     C                   movel     assprv        depprv
073900001113     C                   movel     asscap        depcap
074000001113     C*
074100001113     C* AZZERA I TOTALI DEL CAP
074200001113     C                   Z-ADD     *ZEROS        TCFP1            11 1
074300001113     C                   Z-ADD     *ZEROS        TCFP2            11 1
074400001113     C                   Z-ADD     *ZEROS        TCFP3            11 1
074500001113     C                   Z-ADD     *ZEROS        TCFP4            11 1
074600001113     C                   Z-ADD     *ZEROS        TCFV1            13 3
074700001113     C                   Z-ADD     *ZEROS        TCFV2            13 3
074800001113     C                   Z-ADD     *ZEROS        TCFV3            13 3
074900001113     C                   Z-ADD     *ZEROS        TCFV4            13 3
075000001113     C                   Z-ADD     *ZEROS        TCFC1            11 0
075100001113     C                   Z-ADD     *ZEROS        TCFC2            11 0
075200001113     C                   Z-ADD     *ZEROS        TCFC3            11 0
075300001113     C                   Z-ADD     *ZEROS        TCFC4            11 0
075400001113     C                   Z-ADD     *ZEROS        TCFS1            11 0
075500001113     C                   Z-ADD     *ZEROS        TCFS2            11 0
075600001113     C                   Z-ADD     *ZEROS        TCFS3            11 0
075700001113     C                   Z-ADD     *ZEROS        TCFS4            11 0
075800001113     C                   Z-ADD     *ZEROS        TCTOS            11 0
075900001113     C                   Z-ADD     *ZEROS        TCTOC            11 0
076000001113     C                   Z-ADD     *ZEROS        TCTOP            11 1
076100001113     C                   Z-ADD     *ZEROS        TCTOV            13 3
076200001113     C*
076300001113     C                   ENDSR
076400001106     C*------------------------------------------------------------------------*
076500001106     C* NEWDET - OPERAZIONI PER NUOVO PRV/CAP/LOC
076600001106     C*------------------------------------------------------------------------*
076700001106     C     NEWDET        BEGSR
076800001106     C*
076900001106     C* MEMORIZZA NUOVO DETTAGLIO
077000001120     C                   movel     assloc        deploc
077100001120     C                   movel     assczc        depczc
077200001106     C*
077300001106     C* AZZERA I TOTALI DEL DETTAGLIO
077400001106     C                   Z-ADD     *ZEROS        TLFP1            11 1
077500001106     C                   Z-ADD     *ZEROS        TLFP2            11 1
077600001106     C                   Z-ADD     *ZEROS        TLFP3            11 1
077700001106     C                   Z-ADD     *ZEROS        TLFP4            11 1
077800001106     C                   Z-ADD     *ZEROS        TLFV1            13 3
077900001106     C                   Z-ADD     *ZEROS        TLFV2            13 3
078000001106     C                   Z-ADD     *ZEROS        TLFV3            13 3
078100001106     C                   Z-ADD     *ZEROS        TLFV4            13 3
078200001106     C                   Z-ADD     *ZEROS        TLFC1            11 0
078300001106     C                   Z-ADD     *ZEROS        TLFC2            11 0
078400001106     C                   Z-ADD     *ZEROS        TLFC3            11 0
078500001106     C                   Z-ADD     *ZEROS        TLFC4            11 0
078600001106     C                   Z-ADD     *ZEROS        TLFS1            11 0
078700001106     C                   Z-ADD     *ZEROS        TLFS2            11 0
078800001106     C                   Z-ADD     *ZEROS        TLFS3            11 0
078900001106     C                   Z-ADD     *ZEROS        TLFS4            11 0
079000001109     C                   Z-ADD     *ZEROS        TLTOS            11 0
079100001109     C                   Z-ADD     *ZEROS        TLTOC            11 0
079200001109     C                   Z-ADD     *ZEROS        TLTOP            11 1
079300001109     C                   Z-ADD     *ZEROS        TLTOV            13 3
079400001106     C*
079500001106     C                   ENDSR
079600001106     C*------------------------------------------------------------------------*
079700001106     C* MEMSPE - MEMORIZZA I DATI DI DETTAGLIO
079800001106     C*------------------------------------------------------------------------*
079900001106     C     MEMSPE        BEGSR
080000001106     C*
080100001109     C* TOTALE PRV/CAP/LOC (DETTAGLIO)
080200001106     C                   ADD       assfp1        TLFP1
080300001106     C                   ADD       assfp2        TLFP2
080400001106     C                   ADD       assfp3        TLFP3
080500001106     C                   ADD       assfp4        TLFP4
080600001106     C                   ADD       assfv1        TLFV1
080700001106     C                   ADD       assfv2        TLFV2
080800001106     C                   ADD       assfv3        TLFV3
080900001106     C                   ADD       assfv4        TLFV4
081000001106     C                   ADD       assfc1        TLFC1
081100001106     C                   ADD       assfc2        TLFC2
081200001106     C                   ADD       assfc3        TLFC3
081300001106     C                   ADD       assfc4        TLFC4
081400001106     C                   ADD       assfs1        TLFS1
081500001106     C                   ADD       assfs2        TLFS2
081600001106     C                   ADD       assfs3        TLFS3
081700010507     C                   ADD       assfs4        TLFS4
081800001109     C                   ADD       asstos        TLTOS
081900001109     C                   ADD       asstoc        TLTOC
082000001109     C                   ADD       asstop        TLTOP
082100001109     C                   ADD       asstov        TLTOV
082200001113     C* TOTALE PRV/CAP
082300001113     C                   ADD       assfp1        TCFP1
082400001113     C                   ADD       assfp2        TCFP2
082500001113     C                   ADD       assfp3        TCFP3
082600001113     C                   ADD       assfp4        TCFP4
082700001113     C                   ADD       assfv1        TCFV1
082800001113     C                   ADD       assfv2        TCFV2
082900001113     C                   ADD       assfv3        TCFV3
083000001113     C                   ADD       assfv4        TCFV4
083100001113     C                   ADD       assfc1        TCFC1
083200001113     C                   ADD       assfc2        TCFC2
083300001113     C                   ADD       assfc3        TCFC3
083400001113     C                   ADD       assfc4        TCFC4
083500001113     C                   ADD       assfs1        TCFS1
083600001113     C                   ADD       assfs2        TCFS2
083700001113     C                   ADD       assfs3        TCFS3
083800010507     C                   ADD       assfs4        TCFS4
083900001113     C                   ADD       asstos        TCTOS
084000001113     C                   ADD       asstoc        TCTOC
084100001113     C                   ADD       asstop        TCTOP
084200001113     C                   ADD       asstov        TCTOV
084300001106     C* TOTALE FILIALE
084400001106     C                   ADD       assfp1        TFFP1
084500001106     C                   ADD       assfp2        TFFP2
084600001106     C                   ADD       assfp3        TFFP3
084700001106     C                   ADD       assfp4        TFFP4
084800001106     C                   ADD       assfv1        TFFV1
084900001106     C                   ADD       assfv2        TFFV2
085000001106     C                   ADD       assfv3        TFFV3
085100001106     C                   ADD       assfv4        TFFV4
085200001106     C                   ADD       assfc1        TFFC1
085300001106     C                   ADD       assfc2        TFFC2
085400001106     C                   ADD       assfc3        TFFC3
085500001106     C                   ADD       assfc4        TFFC4
085600001106     C                   ADD       assfs1        TFFS1
085700001106     C                   ADD       assfs2        TFFS2
085800001106     C                   ADD       assfs3        TFFS3
085900010507     C                   ADD       assfs4        TFFS4
086000001109     C                   ADD       asstos        TFTOS
086100001109     C                   ADD       asstoc        TFTOC
086200001109     C                   ADD       asstop        TFTOP
086300001109     C                   ADD       asstov        TFTOV
086400001106     C* TOTALE AREA
086500001106     C                   ADD       assfp1        TAFP1
086600001106     C                   ADD       assfp2        TAFP2
086700001106     C                   ADD       assfp3        TAFP3
086800001106     C                   ADD       assfp4        TAFP4
086900001106     C                   ADD       assfv1        TAFV1
087000001106     C                   ADD       assfv2        TAFV2
087100001106     C                   ADD       assfv3        TAFV3
087200001106     C                   ADD       assfv4        TAFV4
087300001106     C                   ADD       assfc1        TAFC1
087400001106     C                   ADD       assfc2        TAFC2
087500001106     C                   ADD       assfc3        TAFC3
087600001106     C                   ADD       assfc4        TAFC4
087700001106     C                   ADD       assfs1        TAFS1
087800001106     C                   ADD       assfs2        TAFS2
087900001106     C                   ADD       assfs3        TAFS3
088000010507     C                   ADD       assfs4        TAFS4
088100001109     C                   ADD       asstos        TATOS
088200001109     C                   ADD       asstoc        TATOC
088300001109     C                   ADD       asstop        TATOP
088400001109     C                   ADD       asstov        TATOV
088500001106     C* TOTALE DIVISIONE
088600001106     C                   ADD       assfp1        TDFP1
088700001106     C                   ADD       assfp2        TDFP2
088800001106     C                   ADD       assfp3        TDFP3
088900001106     C                   ADD       assfp4        TDFP4
089000001106     C                   ADD       assfv1        TDFV1
089100001106     C                   ADD       assfv2        TDFV2
089200001106     C                   ADD       assfv3        TDFV3
089300001106     C                   ADD       assfv4        TDFV4
089400001106     C                   ADD       assfc1        TDFC1
089500001106     C                   ADD       assfc2        TDFC2
089600001106     C                   ADD       assfc3        TDFC3
089700001106     C                   ADD       assfc4        TDFC4
089800001106     C                   ADD       assfs1        TDFS1
089900001106     C                   ADD       assfs2        TDFS2
090000001106     C                   ADD       assfs3        TDFS3
090100010507     C                   ADD       assfs4        TDFS4
090200001109     C                   ADD       asstos        TDTOS
090300001109     C                   ADD       asstoc        TDTOC
090400001109     C                   ADD       asstop        TDTOP
090500001109     C                   ADD       asstov        TDTOV
090600001114     C* TOTALE GENERALE
090700001106     C                   ADD       assfp1        TGFP1            11 1
090800001106     C                   ADD       assfp2        TGFP2            11 1
090900001106     C                   ADD       assfp3        TGFP3            11 1
091000001106     C                   ADD       assfp4        TGFP4            11 1
091100001106     C                   ADD       assfv1        TGFV1            13 3
091200001106     C                   ADD       assfv2        TGFV2            13 3
091300001106     C                   ADD       assfv3        TGFV3            13 3
091400001106     C                   ADD       assfv4        TGFV4            13 3
091500001106     C                   ADD       assfc1        TGFC1            11 0
091600001106     C                   ADD       assfc2        TGFC2            11 0
091700001106     C                   ADD       assfc3        TGFC3            11 0
091800001106     C                   ADD       assfc4        TGFC4            11 0
091900001106     C                   ADD       assfs1        TGFS1            11 0
092000001106     C                   ADD       assfs2        TGFS2            11 0
092100001106     C                   ADD       assfs3        TGFS3            11 0
092200010507     C                   ADD       assfs4        TGFS4            11 0
092300001109     C                   ADD       asstos        TGTOS            11 0
092400001109     C                   ADD       asstoc        TGTOC            11 0
092500001109     C                   ADD       asstop        TGTOP            11 1
092600001109     C                   ADD       asstov        TGTOV            13 3
092700001106     C*
092800001106     C                   ENDSR
092900001106     C*------------------------------------------------------------------------*
093000001106     C* DECDIV - DECODIFICA DIVISIONE
093100001106     C*------------------------------------------------------------------------*
093200001106     C     DECDIV        BEGSR
093300001106     C*
093400001109     C                   MOVEL     CODUT         KTBUTE
093500001106     C                   MOVE      '17'          KTBCOD
093600001106     C                   MOVE      *BLANKS       KTBKEY
093700001106     C                   MOVEL     DEPDIV        KTBKEY
093800001106     C     KEYTAB        CHAIN     TABEL                              97
093900001106     C     *IN97         IFEQ      '1'
094000001106     C     TBLFLG        OREQ      '*'
094100001106     C                   MOVEL     *ALL'*'       DESDIV           20
094200001106     C                   ELSE
094300001106     C                   MOVEL     TBLUNI        DESDIV
094400001106     C                   ENDIF
094500001106     C*
094600001106     C                   ENDSR
094700001106     C*------------------------------------------------------------------------*
094800001106     C* DECARE - DECODIFICA AREA
094900001106     C*------------------------------------------------------------------------*
095000001106     C     DECARE        BEGSR
095100001106     C*
095200001109     C                   MOVEL     CODUT         KTBUTE
095300001106     C                   MOVE      '05'          KTBCOD
095400001106     C                   MOVE      *BLANKS       KTBKEY
095500001106     C                   MOVEL     DEPARE        KTBKEY
095600001106     C     KEYTAB        CHAIN     TABEL                              97
095700001106     C     *IN97         IFEQ      '1'
095800001106     C     TBLFLG        OREQ      '*'
095900001106     C                   MOVEL     *ALL'*'       DESARE           20
096000001106     C                   ELSE
096100001106     C                   MOVEL     TBLUNI        DESARE
096200001106     C                   ENDIF
096300001106     C*
096400001106     C                   ENDSR
096500001106     C*------------------------------------------------------------------------*
096600001106     C* DECFIL - DECODIFICA FILIALE/TERMINAL
096700001106     C*------------------------------------------------------------------------*
096800001106     C     DECFIL        BEGSR
096900001106     C*
097000001106     C                   Z-ADD     1             I
097100001106     C     DEPFIL        LOOKUP    FIL(I)                                 95
097200001106     C     *IN95         IFEQ      '0'
097300001106     C                   MOVEL     *ALL'*'       DESFIL           20
097400001106     C                   ELSE
097500001106     C                   MOVEL     DFIL(I)       DESFIL
097600001106     C                   ENDIF
097700001106     C*
097800001109     C                   ENDSR
097900001106     C*------------------------------------------------------------------------*
098000001109     C* STADET - STAMPA RIGA DI DETTAGLIO
098100001106     C*------------------------------------------------------------------------*
098200001106     C     STADET        BEGSR
098300001109     C*
098400001113     C                   EVAL      STPTES='P.O. '+%editc(depfil:'Z')+
098500001113     C                                    '  '+desfil
098600001113     C                   EVAL      STPDES = depprv+' '+%subst(depcap:1:5)+
098700001113     C                                      ' '+deploc
098800001120     C                   MOVE      depczc        STPDES
098900001113     C* VALORIZZO ALTRI DATI IN STAMPA
099000001120     C                   EVAL(H)   STPFP1 = TLFP1 / PARGIO / 100
099100001120     C                   EVAL(H)   STPFP2 = TLFP2 / PARGIO / 100
099200001120     C                   EVAL(H)   STPFP3 = TLFP3 / PARGIO / 100
099300001120     C                   EVAL(H)   STPFP4 = TLFP4 / PARGIO / 100
099400001120     C                   EVAL(H)   STPFV1 = TLFV1 / PARGIO
099500001120     C                   EVAL(H)   STPFV2 = TLFV2 / PARGIO
099600001120     C                   EVAL(H)   STPFV3 = TLFV3 / PARGIO
099700001120     C                   EVAL(H)   STPFV4 = TLFV4 / PARGIO
099800001120     C                   EVAL(H)   STPFC1 = TLFC1 / PARGIO
099900001120     C                   EVAL(H)   STPFC2 = TLFC2 / PARGIO
100000001120     C                   EVAL(H)   STPFC3 = TLFC3 / PARGIO
100100001120     C                   EVAL(H)   STPFC4 = TLFC4 / PARGIO
100200001120     C                   EVAL(H)   STPFS1 = TLFS1 / PARGIO
100300001120     C                   EVAL(H)   STPFS2 = TLFS2 / PARGIO
100400001120     C                   EVAL(H)   STPFS3 = TLFS3 / PARGIO
100500001120     C                   EVAL(H)   STPFS4 = TLFS4 / PARGIO
100600001120     C                   EVAL(H)   STPTOS = TLTOS / PARGIO
100700001120     C                   EVAL(H)   STPTOC = TLTOC / PARGIO
100800001120     C                   EVAL(H)   STPTOP = TLTOP / PARGIO / 100
100900001120     C                   EVAL(H)   STPTOV = TLTOV / PARGIO
101000001113     C*
101100001113     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
101200001113     C                   IF        STPTOS > 0
101300001109     C*
101400001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
101500001109     C     *IN01         IFEQ      '1'
101600001109     C                   WRITE     ST45TE                                       *TESTATA
101700001110     C                   SETOFF                                       01OF
101800001109     C                   ENDIF
101900001109     C                   WRITE     ST45DE                                       *DETTAGLIO
102000001110     C                   ENDIF
102100001106     C*
102200001106     C                   ENDSR
102300001113     C*------------------------------------------------------------------------*
102400001113     C* STACAP - STAMPA TOTALI DEL CAP
102500001113     C*------------------------------------------------------------------------*
102600001113     C     STACAP        BEGSR
102700001113     C*
102800001113     C                   EVAL      STPDES='       CAP  '+%subst(depcap:1:5)
102900001113     C* VALORIZZO ALTRI DATI IN STAMPA
103000001120     C                   EVAL(H)   STPFP1 = TCFP1 / PARGIO / 100
103100001120     C                   EVAL(H)   STPFP2 = TCFP2 / PARGIO / 100
103200001120     C                   EVAL(H)   STPFP3 = TCFP3 / PARGIO / 100
103300001120     C                   EVAL(H)   STPFP4 = TCFP4 / PARGIO / 100
103400001120     C                   EVAL(H)   STPFV1 = TCFV1 / PARGIO
103500001120     C                   EVAL(H)   STPFV2 = TCFV2 / PARGIO
103600001120     C                   EVAL(H)   STPFV3 = TCFV3 / PARGIO
103700001120     C                   EVAL(H)   STPFV4 = TCFV4 / PARGIO
103800001120     C                   EVAL(H)   STPFC1 = TCFC1 / PARGIO
103900001120     C                   EVAL(H)   STPFC2 = TCFC2 / PARGIO
104000001120     C                   EVAL(H)   STPFC3 = TCFC3 / PARGIO
104100001120     C                   EVAL(H)   STPFC4 = TCFC4 / PARGIO
104200001120     C                   EVAL(H)   STPFS1 = TCFS1 / PARGIO
104300001120     C                   EVAL(H)   STPFS2 = TCFS2 / PARGIO
104400001120     C                   EVAL(H)   STPFS3 = TCFS3 / PARGIO
104500001120     C                   EVAL(H)   STPFS4 = TCFS4 / PARGIO
104600001120     C                   EVAL(H)   STPTOS = TCTOS / PARGIO
104700001120     C                   EVAL(H)   STPTOC = TCTOC / PARGIO
104800001120     C                   EVAL(H)   STPTOP = TCTOP / PARGIO / 100
104900001120     C                   EVAL(H)   STPTOV = TCTOV / PARGIO
105000001113     C*
105100001113     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
105200001113     C                   IF        STPTOS > 0
105300001113     C*
105400001113     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
105500001113     C     *IN01         IFEQ      '1'
105600001113     C                   WRITE     ST45TE                                       *TESTATA
105700001113     C                   SETOFF                                       01OF
105800001113     C                   ENDIF
105900001113     C                   WRITE     ST45DE                                       *DETTAGLIO
106000001113     C                   ENDIF
106100001113     C*
106200001113     C                   ENDSR
106300001109     C*------------------------------------------------------------------------*
106400001109     C* STAFIL - STAMPA TOTALI DI FILIALE
106500001109     C*------------------------------------------------------------------------*
106600001109     C     STAFIL        BEGSR
106700001109     C*
106800001109     C                   EVAL      STPTES='P.O.        '+%editc(depfil:'Z')+
106900001109     C                                    '  '+desfil
107000001109     C                   EVAL      STPDES='TOTALE P.O. '+%editc(depfil:'Z')+
107100001109     C                                    '  '+desfil
107200001113     C* VALORIZZO ALTRI DATI IN STAMPA
107300001120     C                   EVAL(H)   STPFP1 = TFFP1 / PARGIO / 100
107400001120     C                   EVAL(H)   STPFP2 = TFFP2 / PARGIO / 100
107500001120     C                   EVAL(H)   STPFP3 = TFFP3 / PARGIO / 100
107600001120     C                   EVAL(H)   STPFP4 = TFFP4 / PARGIO / 100
107700001120     C                   EVAL(H)   STPFV1 = TFFV1 / PARGIO
107800001120     C                   EVAL(H)   STPFV2 = TFFV2 / PARGIO
107900001120     C                   EVAL(H)   STPFV3 = TFFV3 / PARGIO
108000001120     C                   EVAL(H)   STPFV4 = TFFV4 / PARGIO
108100001120     C                   EVAL(H)   STPFC1 = TFFC1 / PARGIO
108200001120     C                   EVAL(H)   STPFC2 = TFFC2 / PARGIO
108300001120     C                   EVAL(H)   STPFC3 = TFFC3 / PARGIO
108400001120     C                   EVAL(H)   STPFC4 = TFFC4 / PARGIO
108500001120     C                   EVAL(H)   STPFS1 = TFFS1 / PARGIO
108600001120     C                   EVAL(H)   STPFS2 = TFFS2 / PARGIO
108700001120     C                   EVAL(H)   STPFS3 = TFFS3 / PARGIO
108800001120     C                   EVAL(H)   STPFS4 = TFFS4 / PARGIO
108900001120     C                   EVAL(H)   STPTOS = TFTOS / PARGIO
109000001120     C                   EVAL(H)   STPTOC = TFTOC / PARGIO
109100001120     C                   EVAL(H)   STPTOP = TFTOP / PARGIO / 100
109200001120     C                   EVAL(H)   STPTOV = TFTOV / PARGIO
109300001113     C*
109400001113     C* STAMPO SOLO SE ESISTONO SPEDIZIONI
109500001113     C                   IF        STPTOS > 0
109600001109     C*
109700001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
109800001110     C     *IN01         IFEQ      '1'
109900001109     C                   WRITE     ST45TE                                       *TESTATA
110000001110     C                   SETOFF                                       OF
110100001110     C                   ENDIF
110200001110     C                   SETON                                        01
110300001110     C                   WRITE     ST45RI                                       *SEPARAZIONE
110400001109     C                   WRITE     ST45DE                                       *DETTAGLIO
110500001110     C                   WRITE     ST45RI                                       *SEPARAZIONE
110600001110     C                   ENDIF
110700001109     C*
110800001109     C                   ENDSR
110900001109     C*------------------------------------------------------------------------*
111000001109     C* STAARE - STAMPA TOTALI DI AREA
111100001109     C*------------------------------------------------------------------------*
111200001109     C     STAARE        BEGSR
111300001109     C*
111400001109     C                   EVAL      STPTES='AREA        '+%editc(depare:'Z')+' '+
111500001109     C                                    desare
111600001109     C                   EVAL      STPDES='TOTALE AREA '+%editc(depare:'Z')+' '+
111700001109     C                                    desare
111800001113     C* VALORIZZO ALTRI DATI IN STAMPA
111900001120     C                   EVAL(H)   STPFP1 = TAFP1 / PARGIO / 100
112000001120     C                   EVAL(H)   STPFP2 = TAFP2 / PARGIO / 100
112100001120     C                   EVAL(H)   STPFP3 = TAFP3 / PARGIO / 100
112200001120     C                   EVAL(H)   STPFP4 = TAFP4 / PARGIO / 100
112300001120     C                   EVAL(H)   STPFV1 = TAFV1 / PARGIO
112400001120     C                   EVAL(H)   STPFV2 = TAFV2 / PARGIO
112500001120     C                   EVAL(H)   STPFV3 = TAFV3 / PARGIO
112600001120     C                   EVAL(H)   STPFV4 = TAFV4 / PARGIO
112700001120     C                   EVAL(H)   STPFC1 = TAFC1 / PARGIO
112800001120     C                   EVAL(H)   STPFC2 = TAFC2 / PARGIO
112900001120     C                   EVAL(H)   STPFC3 = TAFC3 / PARGIO
113000001120     C                   EVAL(H)   STPFC4 = TAFC4 / PARGIO
113100001120     C                   EVAL(H)   STPFS1 = TAFS1 / PARGIO
113200001120     C                   EVAL(H)   STPFS2 = TAFS2 / PARGIO
113300001120     C                   EVAL(H)   STPFS3 = TAFS3 / PARGIO
113400001120     C                   EVAL(H)   STPFS4 = TAFS4 / PARGIO
113500001120     C                   EVAL(H)   STPTOS = TATOS / PARGIO
113600001120     C                   EVAL(H)   STPTOC = TATOC / PARGIO
113700001120     C                   EVAL(H)   STPTOP = TATOP / PARGIO / 100
113800001120     C                   EVAL(H)   STPTOV = TATOV / PARGIO
113900001113     C*
114000001113     C* PROSEGUO E STAMPO SOLO SE ESISTONO SPEDIZIONI
114100001113     C                   IF        STPTOS > 0
114200001109     C*
114300001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
114400001110     C     *IN01         IFEQ      '1'
114500001110     C                   WRITE     ST45TE                                       *TESTATA
114600001110     C                   SETOFF                                       OF
114700001110     C                   ENDIF
114800001110     C                   SETON                                        01
114900001110     C                   WRITE     ST45RI                                       *SEPARAZIONE
115000001110     C                   WRITE     ST45DE                                       *DETTAGLIO
115100001110     C                   WRITE     ST45RI                                       *SEPARAZIONE
115200001110     C                   ENDIF
115300001109     C*
115400001109     C                   ENDSR
115500001109     C*------------------------------------------------------------------------*
115600001109     C* STADIV - STAMPA TOTALI DI DIVISIONE
115700001109     C*------------------------------------------------------------------------*
115800001109     C     STADIV        BEGSR
115900001109     C*
116000001109     C                   EVAL      STPTES='DIVISIONE        '+depdiv+' '+desdiv
116100001109     C                   EVAL      STPDES='TOTALE DIVIONE   '+depdiv+' '+desdiv
116200001113     C* VALORIZZO ALTRI DATI IN STAMPA
116300001120     C                   EVAL(H)   STPFP1 = TDFP1 / PARGIO / 100
116400001120     C                   EVAL(H)   STPFP2 = TDFP2 / PARGIO / 100
116500001120     C                   EVAL(H)   STPFP3 = TDFP3 / PARGIO / 100
116600001120     C                   EVAL(H)   STPFP4 = TDFP4 / PARGIO / 100
116700001120     C                   EVAL(H)   STPFV1 = TDFV1 / PARGIO
116800001120     C                   EVAL(H)   STPFV2 = TDFV2 / PARGIO
116900001120     C                   EVAL(H)   STPFV3 = TDFV3 / PARGIO
117000001120     C                   EVAL(H)   STPFV4 = TDFV4 / PARGIO
117100001120     C                   EVAL(H)   STPFC1 = TDFC1 / PARGIO
117200001120     C                   EVAL(H)   STPFC2 = TDFC2 / PARGIO
117300001120     C                   EVAL(H)   STPFC3 = TDFC3 / PARGIO
117400001120     C                   EVAL(H)   STPFC4 = TDFC4 / PARGIO
117500001120     C                   EVAL(H)   STPFS1 = TDFS1 / PARGIO
117600001120     C                   EVAL(H)   STPFS2 = TDFS2 / PARGIO
117700001120     C                   EVAL(H)   STPFS3 = TDFS3 / PARGIO
117800001120     C                   EVAL(H)   STPFS4 = TDFS4 / PARGIO
117900001120     C                   EVAL(H)   STPTOS = TDTOS / PARGIO
118000001120     C                   EVAL(H)   STPTOC = TDTOC / PARGIO
118100001120     C                   EVAL(H)   STPTOP = TDTOP / PARGIO / 100
118200001120     C                   EVAL(H)   STPTOV = TDTOV / PARGIO
118300001113     C*
118400001113     C* PROSEGUO E STAMPO SOLO SE ESISTONO SPEDIZIONI
118500001113     C                   IF        STPTOS > 0
118600001109     C*
118700001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
118800001110     C     *IN01         IFEQ      '1'
118900001110     C                   WRITE     ST45TE                                       *TESTATA
119000001110     C                   SETOFF                                       OF
119100001110     C                   ENDIF
119200001110     C                   SETON                                        01
119300001110     C                   WRITE     ST45RI                                       *SEPARAZIONE
119400001109     C                   WRITE     ST45DE                                       *DETTAGLIO
119500001110     C                   WRITE     ST45RI                                       *SEPARAZIONE
119600001110     C                   ENDIF
119700001109     C*
119800001109     C                   ENDSR
119900001109     C*------------------------------------------------------------------------*
120000001109     C* STAGEN - STAMPA TOTALI GENERALI
120100001109     C*------------------------------------------------------------------------*
120200001109     C     STAGEN        BEGSR
120300001109     C*
120400001110     C                   EVAL      STPDIV = *BLANKS
120500001110     C                   EVAL      STPARE = *BLANKS
120600001110     C                   EVAL      STPTES = *BLANKS
120700001109     C                   EVAL      STPDES = 'TOTALE GENERALE'
120800001109     C*
120900001120     C                   EVAL(H)   STPFP1 = TGFP1 / PARGIO / 100
121000001120     C                   EVAL(H)   STPFP2 = TGFP2 / PARGIO / 100
121100001120     C                   EVAL(H)   STPFP3 = TGFP3 / PARGIO / 100
121200001120     C                   EVAL(H)   STPFP4 = TGFP4 / PARGIO / 100
121300001120     C                   EVAL(H)   STPFV1 = TGFV1 / PARGIO
121400001120     C                   EVAL(H)   STPFV2 = TGFV2 / PARGIO
121500001120     C                   EVAL(H)   STPFV3 = TGFV3 / PARGIO
121600001120     C                   EVAL(H)   STPFV4 = TGFV4 / PARGIO
121700001120     C                   EVAL(H)   STPFC1 = TGFC1 / PARGIO
121800001120     C                   EVAL(H)   STPFC2 = TGFC2 / PARGIO
121900001120     C                   EVAL(H)   STPFC3 = TGFC3 / PARGIO
122000001120     C                   EVAL(H)   STPFC4 = TGFC4 / PARGIO
122100001120     C                   EVAL(H)   STPFS1 = TGFS1 / PARGIO
122200001120     C                   EVAL(H)   STPFS2 = TGFS2 / PARGIO
122300001120     C                   EVAL(H)   STPFS3 = TGFS3 / PARGIO
122400001120     C                   EVAL(H)   STPFS4 = TGFS4 / PARGIO
122500001120     C                   EVAL(H)   STPTOS = TGTOS / PARGIO
122600001120     C                   EVAL(H)   STPTOC = TGTOC / PARGIO
122700001120     C                   EVAL(H)   STPTOP = TGTOP / PARGIO / 100
122800001120     C                   EVAL(H)   STPTOV = TGTOV / PARGIO
122900001109     C*
123000001109     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
123100001110     C     *IN01         IFEQ      '1'
123200001110     C                   WRITE     ST45TE                                       *TESTATA
123300001110     C                   SETOFF                                       OF
123400001110     C                   ENDIF
123500001110     C                   WRITE     ST45RI                                       *SEPARAZIONE
123600001109     C                   WRITE     ST45DE                                       *DETTAGLIO
123700001109     C                   WRITE     ST45RI                                       *SEPARAZIONE
123800001109     C                   WRITE     ST45FI                                       *FINE
123900001109     C*
124000001109     C                   ENDSR
124100000914     C*------------------------------------------------------------------------*
124200000914     C* DECODI - Routine di controllo validità record
124300000914     C*------------------------------------------------------------------------*
124400000914     C     DECODI        BEGSR
124500000914     C*
124600001016     C                   movel     '0'           errl00            1
124700001030     C                   movel     *blanks       chkind
124800001016     C*--------------------
124900001025     C* ESCLUDE LE BOLLE NON VALIDE COME "RICAVO"
125000001016     C*--------------------
125100001016     C                   if        errl00 = '0'
125200001016     C                   z-add     1             j
125300001025     C     tastbl        lookup    TB(j)                                  28
125400001025     C                   if        %found and RBL(j) = 'N'
125500001025     C                   else
125600001025     C                   movel     '1'           errl00
125700001025     C                   endif
125800001025     C                   endif
125900001106     C*--------------------
126000001106     C* CONTROLLO NETWORK RICHIESTI
126100001106     C*--------------------
126200001106     C                   if        errl00 = '0'
126300001106     C                   if        PARNT1 = *blanks and
126400001106     C                             PARNT2 = *blanks and
126500001106     C                             PARNT3 = *blanks and
126600001106     C                             PARNT4 = *blanks and
126700001106     C                             PARNT5 = *blanks and
126800001106     C                             PARNT6 = *blanks
126900001106     C                   else
127000001110     C                   z-add     1             i
127100001110     C     taslnp        lookup    fil(i)                                 99
127200001110     C                   if        %found
127300001110     C                   movel     ntx(i)        depntw
127400001110     C                   else
127500020226     C                   movel     *all'X'       depntw
127600001110     C                   endif
127700001110     C                   if        depntw = PARNT1 or
127800001110     C                             depntw = PARNT2 or
127900001110     C                             depntw = PARNT3 or
128000001110     C                             depntw = PARNT4 or
128100001110     C                             depntw = PARNT5 or
128200001110     C                             depntw = PARNT6
128300001106     C                   else
128400001106     C                   movel     '1'           errl00
128500001106     C                   endif
128600001106     C                   endif
128700001106     C                   endif
128800000914     C*--------------------
128900001016     C* CONTROLLO TERMINAL PARTENZA / ARRIVO
129000000914     C*--------------------
129100001016     C                   if        errl00 = '0'
129200001016     C                   if        PARTP1 > *zeros or
129300001016     C                             PARTP2 > *zeros or
129400001016     C                             PARTP3 > *zeros or
129500001016     C                             PARTP4 > *zeros or
129600001016     C                             PARTP5 > *zeros or
129700001016     C                             PARTP6 > *zeros or
129800001016     C                             PARTP7 > *zeros or
129900001016     C                             PARTP8 > *zeros or
130000001016     C                             PARTP9 > *zeros or
130100001016     C                             PARTP0 > *zeros
130200001016     C                   if        tastfp = PARTP1 or
130300001016     C                             tastfp = PARTP2 or
130400001016     C                             tastfp = PARTP3 or
130500001016     C                             tastfp = PARTP4 or
130600001016     C                             tastfp = PARTP5 or
130700001016     C                             tastfp = PARTP6 or
130800001016     C                             tastfp = PARTP7 or
130900001016     C                             tastfp = PARTP8 or
131000001016     C                             tastfp = PARTP9 or
131100001016     C                             tastfp = PARTP0 or
131200001016     C                             tastfa = PARTP1 or
131300001016     C                             tastfa = PARTP2 or
131400001016     C                             tastfa = PARTP3 or
131500001016     C                             tastfa = PARTP4 or
131600001016     C                             tastfa = PARTP5 or
131700001016     C                             tastfa = PARTP6 or
131800001016     C                             tastfa = PARTP7 or
131900001016     C                             tastfa = PARTP8 or
132000001016     C                             tastfa = PARTP9 or
132100001016     C                             tastfa = PARTP0
132200001016     C                   movel     '0'           errl00
132300001016     C                   else
132400001016     C                   movel     '1'           errl00
132500001016     C                   endif
132600001016     C                   endif
132700001016     C                   endif
132800001016     C*--------------------
132900001030     C* CONTROLLO NAZIONE PARTENZA/ARRIVO E CAP PARTENZA/ARRIVO
133000001016     C*--------------------
133100001016     C                   if        errl00 = '0'
133200001016     C* ... provo prima sul CNIND per i clienti codificati
133300001016     C*
133400001016     C* REPERIMENTO DATI JOB
133500001016     C                   movel     tasccm        depfil
133600001016     C                   move(p)   tasccm        depcli
133700001016     C                   if        depcli = 8888 or depcli = 9999
133800001016     C                   movel     '1'           O69ERR
133900001016     C                   else
134000001016     C                   clear                   BS69DS
134100001016     C                   clear                   ACODS
134200001030     C                   clear                   INDDS
134300001030     C                   clear                   CLPDS
134400001030     C                   clear                   CLSDS
134500020802     C                   movel     KNSIF         I69SIF                         *S.I.
134600001016     C                   z-add     tasccm        I69KIN                         *CLIENTE X CNIND
134700001016     C                   call      'TIBS69R'
134800001016     C                   parm                    BS69DS
134900001016     C                   parm                    ACODS
135000001016     C                   parm                    INDDS
135100001016     C                   parm                    CLPDS
135200001016     C                   parm                    CLSDS
135300001016     C                   endif
135400001016     C                   if        O69ERR <> '1'
135500001016     C                   movel     '1'           chkind
135600001016     C                   movel     INDCAP        wrkcap
135700001016     C                   if        PARNAZ <> '999'
135800001016     C                   if        tasnzd = PARNAZ or
135900001016     C                             %subst(indsta:1:3) = PARNAZ
136000001016     C                   else
136100001016     C                   movel     '1'           errl00
136200001016     C                   endif
136300001110     C                   endif
136400001110     C                   if        PARCAD > *blanks
136500001110     C                   if        %subst(tascad:1:2) >= PARCAD or
136600001110     C                             wrkcap >= PARCAD
136700001110     C                   else
136800001110     C                   movel     '1'           errl00
136900001110     C                   endif
137000001110     C                   endif
137100001110     C                   if        PARCAA > *blanks
137200001110     C                   if        %subst(tascad:1:2) <= PARCAA or
137300001110     C                             wrkcap <= PARCAA
137400001110     C                   else
137500001110     C                   movel     '1'           errl00
137600001110     C                   endif
137700001016     C                   endif
137800001016     C                   else
137900001016     C* ... poi provo su TITAA per i clienti non codificati
138000001016     C                   z-add     tasaas        kaAaas
138100001016     C                   z-add     taslnp        kaAlnp
138200001016     C                   z-add     tasnrs        kaAnrs
138300001016     C                   z-add     tasnsp        kaAnsp
138400001016     C                   movel     'M'           kaAtrc
138500001016     C     keytaA        chain     titaA30c                           99
138600001016     C                   if        %found(titaA30c)
138700001016     C                   if        PARNAZ <> '999'
138800001016     C                   if        tasnzd = PARNAZ or
138900001016     C                             taanaz = PARNAZ
139000001016     C                   else
139100001016     C                   movel     '1'           errl00
139200001016     C                   endif
139300001110     C                   endif
139400001110     C                   if        PARCAD > *blanks
139500001110     C                   if        %subst(tascad:1:2) >= PARCAD or
139600001110     C                             %subst(taacap:1:2) >= PARCAD
139700001110     C                   else
139800001110     C                   movel     '1'           errl00
139900001110     C                   endif
140000001110     C                   endif
140100001110     C                   if        PARCAA > *blanks
140200001110     C                   if        %subst(tascad:1:2) <= PARCAA or
140300001110     C                             %subst(taacap:1:2) <= PARCAA
140400001110     C                   else
140500001110     C                   movel     '1'           errl00
140600001110     C                   endif
140700001016     C                   endif
140800001016     C                   else
140900001016     C                   movel     '1'           errl00
141000001016     C                   endif
141100001016     C                   endif
141200001016     C                   endif
141300001030     C*
141400000914     C                   ENDSR
141500001113     C*------------------------------------------------------------------------*
141600001113     C* CREAP - CREA IL RECORD SUL FILE DI LAVORO PER I DATI DI "PARTENZA"
141700001113     C*------------------------------------------------------------------------*
141800001113     C     CREAP         BEGSR
141900001113     C*
142000001113     C*-------------------------
142100001113     C* Imposto la chiave
142200001113     C*-------------------------
142300001113     C                   movel     'P'           kwaver
142400001113     C                   z-add     1             i
142500001113     C     taslnp        lookup    fil(i)                                 99
142600001113     C                   if        %found
142700001113     C                   movel     ntx(i)        kwantw
142800001113     C                   else
142900020226     C                   movel     *all'X'       kwantw
143000001113     C                   endif
143100001113     C                   if        chkind = '1'
143200001113     C                   eval      kwanaz = %subst(indsta:1:3)
143300001113     C                   eval      kwaprv = indprv
143400001113     C                   movel(p)  indcap        depcap
143500001113     C                   movel(p)  indcit        deploc
143600001113     C                   else
143700001113     C                   eval      kwanaz = taanaz
143800001113     C                   eval      kwaprv = taaprv
143900001113     C                   movel(p)  taacap        depcap
144000001113     C                   movel(p)  taaloc        deploc
144100001113     C                   endif
144200001113     C                   z-add     taslnp        kwalnx
144300001113     C                   z-add     taslnp        kwalnp
144400001113     C                   z-add     taslna        kwalna
144500001113     C                   z-add     kwalnx        depcla
144600001113     C                   z-add     *zeros        depczc
144700001113     C* Normalizzo la località mediante il TISI95
144800001113     C                   exsr      normloc
144900001113  ***C                   movel(p)  depcap        kwacap
145000001113     C                   movel(p)  deploc        kwaloc
145100001113     C*
145200001113     C* Mi posiziono per sapere se inserire o aggiornare.
145300001113     C     keywfa        chain     wfass01l                           99
145400001113     C*
145500001113     C* Riempio il buffer del record (secondo ben determinate condizioni)
145600001113     C   99              clear                   wfass000                       *RE-INIZIALIZZA BUFF
145700001113     C                   exsr      addwfp                                       *VALORIZZA
145800001113     C   99              write     wfass000                                     *RECORD NON ESISTE,
145900001113     C  N99              update    wfass000                                     *RECORD ESISTE, AGGI
146000001113     C*
146100001113     C                   ENDSR
146200001113     C*------------------------------------------------------------------------*
146300001113     C* CREAA - CREA IL RECORD SUL FILE DI LAVORO PER I DATI DI "ARRIVO"
146400001113     C*------------------------------------------------------------------------*
146500001113     C     CREAA         BEGSR
146600001113     C*
146700001113     C*-------------------------
146800001113     C* Imposto la chiave
146900001113     C*-------------------------
147000001113     C                   movel     'A'           kwaver
147100001113     C                   z-add     1             i
147200001114     C*    taslna        lookup    fil(i)                                 99
147300001114     C     taslnp        lookup    fil(i)                                 99
147400001113     C                   if        %found
147500001113     C                   movel     ntx(i)        kwantw
147600001113     C                   else
147700020226     C                   movel     *all'X'       kwantw
147800001113     C                   endif
147900001113     C                   movel     tasnzd        kwanaz
148000001113     C                   movel     tasprd        kwaprv
148100001113     C                   movel     tascad        depcap
148200001113     C                   movel     taslod        deploc
148300001113     C                   z-add     taslna        kwalnx
148400001113     C                   z-add     taslnp        kwalnp
148500001113     C                   z-add     taslna        kwalna
148600001113     C                   z-add     kwalnx        depcla
148700001113     C                   z-add     *zeros        depczc
148800001113     C* Normalizzo la località mediante il TISI95
148900001113     C                   exsr      normloc
149000001113  ***C                   movel(p)  depcap        kwacap
149100001113     C                   movel(p)  deploc        kwaloc
149200001113     C*
149300001113     C* Mi posiziono per sapere se inserire o aggiornare.
149400001113     C     keywfa        chain     wfass01l                           99
149500001113     C*
149600001113     C* Riempio il buffer del record (secondo ben determinate condizioni)
149700001113     C   99              clear                   wfass000                       *RE-INIZIALIZZA BUFF
149800001113     C                   exsr      addwfp                                       *VALORIZZA
149900001113     C   99              write     wfass000                                     *RECORD NON ESISTE,
150000001113     C  N99              update    wfass000                                     *RECORD ESISTE, AGGI
150100001113     C*
150200001113     C                   ENDSR
150300001113     C*------------------------------------------------------------------------*
150400001113     C* NORMLOC - Normalizza le località mediante il TISI95
150500001113     C*------------------------------------------------------------------------*
150600001113     C     NORMLOC       BEGSR
150700001113     C*
150800001113     C                   clear                   dssi95                         *azzera i parametri
150900001113     C                   movel     *blanks       i95tla                         *tipo lancio
151000001113     C                   movel     '7'           i95tcn                         *tipo controllo
151100001113     C                   movel     tastsp        i95tsp                         *tipo servizio
151200001113     C                   movel     kwanaz        i95nar                         *nazione
151300001113     C                   movel     depcap        i95cap                         *cap
151400001113     C                   movel     deploc        i95loc                         *località
151500001113     C                   movel     kwaprv        i95prv                         *provincia
151600010308     C                   if        PARDRI > *zeros
151700010308     C                   z-add     PARDRI        i95dat                         *data di riferimento
151800001120     C                   else
151900001120     C                   z-add     dwdat         i95dat                         *data di riferimento
152000020226     C                   endif
152100020226     C*
152200020226     C* VERIFICO SE LNA E' FILIALE FEDEX
152300020226     C                   if        kwalna > *zeros
152400020226     C     kwalna        lookup    FILFED                                 55
152500020226     C                   if        *in55 = *on
152600020226     C                   eval      i95fi1 = 'S'
152700020226     C                   endif
152800020226     C                   endif
152900020226     C*
153000001121     C                   call      'TISI95R'
153100001113     C                   parm                    dssi95
153200001113     C*
153300001113     C                   if        o95loc > *blanks
153400001113     C                   movel     o95cap        depcap
153500001113     C                   movel     o95loc        deploc
153600001121     C                   z-add     o95lna        depcla
153700001121     C                   z-add     o95znc        depczc
153800001113     C                   endif
153900001113     C*
154000001113     C                   ENDSR
154100001113     C*------------------------------------------------------------------------*
154200001113     C* ADDWFP - Aggiunge la bolla
154300001113     C*------------------------------------------------------------------------*
154400001113     C     ADDWFP        BEGSR
154500001113     C*
154600001113     C                   movel     kwaver        assver
154700001113     C                   z-add     kwalnx        asslnx
154800001113     C                   movel     kwanaz        assnaz
154900001113     C                   movel     kwaprv        assprv
155000001113     C                   movel     kwacap        asscap
155100001113     C                   movel     kwaloc        assloc
155200001113     C                   z-add     kwalna        asslna
155300001113     C                   z-add     kwalnp        asslnp
155400001113     C                   movel     kwantw        assntw
155500001113     C                   z-add     depcla        asscla
155600001113     C                   z-add     depczc        assczc
155700001113     C*
155800001113     C* Valorizzo le date di lancio passate a video
155900001113     C                   z-add     PARDAI        assdai
156000001113     C                   z-add     PARDAF        assdaf
156100001113     C*
156200001113     C* Valorizzo i dati relativi alla classificazione territoriale (sia rispetto alla LNP/LNA)
156300001113     C                   z-add     1             i
156400001113     C                   dow       i<=500
156500001113     C                   if        asslnx = FIL(i)
156600001113     C                   z-add     ARE(i)        assare
156700001113     C                   movel     DIV(i)        assdiv
156800001113     C                   leave
156900001113     C                   endif
157000001113     C                   add       1             i
157100001113     C                   enddo
157200001114     C*                  if        assver = 'A'
157300001114     C*                  z-add     tastfa        asster
157400001114     C*                  else
157500001113     C                   z-add     tastfp        asster
157600001114     C*                  endif
157700001113     C*
157800001113     C* Valorizzo i campi relativi ai dati di instradamento standard teorici del CAP
157900001113     C                   z-add     1             i
158000001113     C                   dow       i<=500
158100001113     C                   if        asscla = FIL(i)
158200001113     C                   z-add     ARE(i)        asscar
158300001113     C                   movel     DIV(i)        asscdi
158400001113     C                   leave
158500001113     C                   endif
158600001113     C                   add       1             i
158700001113     C                   enddo
158800001113     C*
158900001113     C* Valorizzo i dati relativi al contenuto della bolla (colli / peso / volume / spedizioni)
159000001113     C* Considerazioni sul peso e/o volume
159100001113     C                   if        PARKG1 > *zeros  and
159200001113     C                             taspkf >= *zeros and
159300001113     C                             taspkf <  PARKG1 or
159400001113     C                             PARMC1 >  *zeros and
159500001113     C                             tasvlf >= *zeros and
159600001113     C                             tasvlf <  PARMC1
159700001113     C                   add       tasncl        assfc1
159800001113     C                   add       tasncl        asstoc
159900001113     C                   add       1             assfs1
160000001113     C                   add       1             asstos
160100001113     C                   add       taspkf        assfp1
160200001113     C                   add       taspkf        asstop
160300001113     C                   add       tasvlf        assfv1
160400001113     C                   add       tasvlf        asstov
160500001113     C                   else
160600001113     C                   if        PARKG2 > *zeros  and
160700001113     C                             taspkf >= PARKG1 and
160800001113     C                             taspkf <  PARKG2 or
160900001113     C                             PARMC2 >  *zeros and
161000001113     C                             tasvlf >= PARMC1 and
161100001113     C                             tasvlf <  PARMC2
161200001113     C                   add       tasncl        assfc2
161300001113     C                   add       tasncl        asstoc
161400001113     C                   add       1             assfs2
161500001113     C                   add       1             asstos
161600001113     C                   add       taspkf        assfp2
161700001113     C                   add       taspkf        asstop
161800001113     C                   add       tasvlf        assfv2
161900001113     C                   add       tasvlf        asstov
162000001113     C                   else
162100001113     C                   if        PARKG3 >  *zeros and
162200001113     C                             taspkf >= PARKG2 and
162300001113     C                             taspkf <  PARKG3 or
162400001113     C                             PARMC3 >  *zeros and
162500001113     C                             tasvlf >= PARMC2 and
162600001113     C                             tasvlf <  PARMC3
162700001113     C                   add       tasncl        assfc3
162800001113     C                   add       tasncl        asstoc
162900001113     C                   add       1             assfs3
163000001113     C                   add       1             asstos
163100001113     C                   add       taspkf        assfp3
163200001113     C                   add       taspkf        asstop
163300001113     C                   add       tasvlf        assfv3
163400001113     C                   add       tasvlf        asstov
163500001113     C                   else
163600001113     C                   if        (PARKG4 = 999      and
163700001113     C                              taspkf >= PARKG3  or
163800001113     C                              PARMC4 = 9,99     and
163900001113     C                              tasvlf >= PARMC3) or
164000001113     C                             (PARKG4 > *zeros  and
164100001113     C                              taspkf >= PARKG3 and
164200001113     C                              taspkf <  PARKG4 or
164300001113     C                              PARMC4 >  *zeros and
164400001113     C                              tasvlf >= PARMC3 and
164500001113     C                              tasvlf <  PARMC4)
164600001113     C                   add       tasncl        assfc4
164700001113     C                   add       tasncl        asstoc
164800001113     C                   add       1             assfs4
164900001113     C                   add       1             asstos
165000001113     C                   add       taspkf        assfp4
165100001113     C                   add       taspkf        asstop
165200001113     C                   add       tasvlf        assfv4
165300001113     C                   add       tasvlf        asstov
165400001113     C                   endif
165500001113     C                   endif
165600001113     C                   endif
165700001113     C                   endif
165800001113     C*
165900001113     C* Altri dati
166000001113     C                   movel     o95lia        asslia
166100001113     C*
166200001113     C                   ENDSR
166300000914     C*------------------------------------------------------------------------*
166400000914     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
166500000914     C*------------------------------------------------------------------------*
166600000914     C     CARORG        BEGSR
166700020226     C*
166800020226     C                   Z-ADD     *ZEROS        K                 3 0
166900000914     C*
167000000914     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
167100000914     C                   Z-ADD     *ZEROS        KORFIL
167200000914     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
167300000914     C  N99              READ      AZORG01L                               99
167400000915     C                   Z-ADD     0             y                              *INDICE SCHIERA
167500000914     C     *IN99         DOWEQ     '0'                                          --- 1 -->
167600000914     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
167700000914     C     ORGFAG        ANDNE     'V'
167800000915     C                   ADD       1             y
167900000915     C                   Z-ADD     ORGFIL        FIL(y)
168000001106     C                   MOVEL     ORGDES        DFIL(y)
168100000915     C                   Z-ADD     ORGCAR        ARE(y)
168200000915     C                   MOVEL     ORGFL3        DIV(y)
168300000914     C                   MOVEL     ORGDE3        OG143
168400001016     C* CALCOLA NETWORK
168500020226     C                   MOVEL     §OGNTW        NTX(y)                         *NETWORK
168600020226     C* VERIFICO SE FILIALE FEDEX
168700020226     C                   IF        §OGNTW = 'FED'
168800020226     C                   ADD       1             K
168900020226     C                   Z-ADD     ORGFIL        FILFED(K)
169000020226     C                   ENDIF
169100001016     C                   ENDIF
169200000914     C                   READ      AZORG01L                               99
169300001016     C                   ENDDO
169400000914     C*
169500000914     C                   ENDSR
169600000000     C*------------------------------------------------------------------------*
169700000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
169800000000     C*------------------------------------------------------------------------*
169900000000     C     CARTBL        BEGSR
170000000000     C*--------------------
170100000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
170200000000     C*--------------------
170300001025     C*                  Z-ADD     0             I
170400001025     C*                  Z-ADD     CODUT         KTBUTE
170500001025     C*                  MOVEL     '2W'          KTBCOD
170600001025     C*                  MOVEL     *BLANKS       KTBKEY
170700001025     C*    KEYTAB        SETLL     TABEL00F
170800001025     C*                  READ      TABEL00F                               99
170900001025     C*    *IN99         DOWEQ     *OFF
171000001025     C*    TBLCOD        ANDEQ     '2W'
171100001025     C*    TBLFLG        IFEQ      *BLANKS
171200001025     C*                  ADD       1             I
171300001025     C*                  MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
171400001025     C*                  MOVEL     TBLUNI        DS2W
171500001025     C*                  MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
171600001025     C*                  MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
171700001025     C*                  ENDIF
171800001025     C*                  READ      TABEL00F                               99
171900001025     C*                  ENDDO
172000000000     C*--------------------
172100000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
172200000000     C*--------------------
172300000000     C                   Z-ADD     0             I
172400000000     C                   Z-ADD     CODUT         KTBUTE
172500000000     C                   MOVEL     'TB'          KTBCOD
172600000000     C                   MOVEL     *BLANKS       KTBKEY
172700000000     C     KEYTAB        SETLL     TABEL00F
172800000000     C                   READ      TABEL00F                               99
172900000000     C     *IN99         DOWEQ     *OFF
173000000000     C     TBLCOD        ANDEQ     'TB'
173100000000     C     TBLFLG        IFEQ      *BLANKS
173200000000     C                   ADD       1             I
173300000000     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
173400000000     C                   MOVEL     TBLUNI        DSTB
173500001025     C                   MOVEL     §TBRBL        RBL(I)
173600001025     C*                  Z-ADD     1             J
173700001025     C*                  SETOFF                                       28
173800001025     C*    §TBRBL        LOOKUP    RBL(J)                                 28
173900001025     C*    *IN28         IFEQ      *ON
174000001025     C*                  MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
174100001025     C*                  MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
174200001025     C*                  ELSE
174300001025     C*                  MOVEL     'N'           SP(I)
174400001025     C*                  MOVEL     'N'           RI(I)
174500001025     C*                  ENDIF
174600001016     C                   ENDIF
174700000000     C                   READ      TABEL00F                               99
174800001016     C                   ENDDO
174900000000     C*
175000000000     C                   ENDSR
175100001109     C*--------------------------------------------------------------------------------------------*
175200001109     C* CHIUDE - Operazioni di chiusura
175300001109     C*--------------------------------------------------------------------------------------------*
175400001109     C     CHIUDE        BEGSR
175500001109     C*
175600001109     C* Programmi
175700001109     C                   clear                   dssi95
175800001109     C                   movel     'C'           i95tla                         *TIPO LANCIO
175900001109     C                   call      'TISI95R'
176000001109     C                   parm                    dssi95
176100001109     C*
176200001109     C                   clear                   BS69DS
176300001109     C                   clear                   ACODS
176400001109     C                   clear                   INDDS
176500001109     C                   clear                   CLPDS
176600001109     C                   clear                   CLSDS
176700001109     C                   movel     'C'           I69TLA                         *TIPO LANCIO
176800020802     C                   movel     KNSIF         I69SIF                         *S.INFORMATIVO
176900001109     C                   call      'TIBS69R'
177000001109     C                   parm                    BS69DS
177100001109     C                   parm                    ACODS
177200001109     C                   parm                    INDDS
177300001109     C                   parm                    CLPDS
177400001109     C                   parm                    CLSDS
177500001109     C*
177600001109     C                   ENDSR
177700000000     C*------------------------------------------------------------------------*
177800000000     C* *INZSR - ROUTINE INIZIALE
177900000000     C*------------------------------------------------------------------------*
178000000000     C     *INZSR        BEGSR
178100000000     C*--------------------
178200000000     C* RICEVIMENTO PARAMETRI
178300000000     C*--------------------
178400000000     C     *ENTRY        PLIST
178500000000     C                   PARM                    KPJBA
178600010417     C                   MOVEL(p)  KPJBU         PARAM
178700000000     C*--------------------
178800000000     C* CHIAVI DI LETTURA
178900000000     C*--------------------
179000000000     C*
179100000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
179200000000     C     KEYORG        KLIST
179300000000     C                   KFLD                    KORFIL
179400000000     C*
179500000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
179600000000     C     KEYTAB        KLIST
179700000000     C                   KFLD                    KTBUTE
179800000000     C                   KFLD                    KTBCOD
179900000000     C                   KFLD                    KTBKEY
180000000914     C*
180100001113     C* CHIAVE LETTURA WFASS01L - COMPLETA (PER SCRITTURA)
180200001016     C     KEYWFA        KLIST
180300001110     C                   KFLD                    KWAVER
180400001113     C                   KFLD                    KWALNX
180500001113     C                   KFLD                    KWALNA
180600001113     C                   KFLD                    KWALNP
180700001016     C                   KFLD                    KWANAZ
180800001016     C                   KFLD                    KWAPRV
180900001110     C                   KFLD                    KWACAP
181000001016     C                   KFLD                    KWALOC
181100001110     C                   KFLD                    KWANTW
181200001110     C*
181300001113     C* CHIAVE LETTURA WFASS01L - PARZIALE (PER STAMPA)
181400001110     C     KEYWF2        KLIST
181500001110     C                   KFLD                    KWAVER
181600001110     C                   KFLD                    KWACDI
181700001110     C                   KFLD                    KWACAR
181800001110     C                   KFLD                    KWACLA
181900001110     C                   KFLD                    KWANAZ
182000001110     C                   KFLD                    KWAPRV
182100001110     C                   KFLD                    KWACAP
182200001110     C                   KFLD                    KWALOC
182300001113     C                   KFLD                    KWACZC
182400001016     C*
182500001016     C* CHIAVE LETTURA TITAS38C - COMPLETA
182600001016     C     KEYTAS        KLIST                                                  titas38c
182700001016     C                   KFLD                    ktaaas                         -anno
182800001016     C                   KFLD                    ktamgs                         -mese/giorno
182900001016     C                   KFLD                    ktaksc                         -cliente
183000001016     C*
183100001016     C* CHIAVE LETTURA TITAA30C - COMPLETA
183200001016     C     KEYTAA        KLIST                                                  titaA30c
183300001016     C                   KFLD                    kaAaas                         -anno spedizione
183400001016     C                   KFLD                    kaAlnp                         -linea partenza
183500001016     C                   KFLD                    kaAnrs                         -serie
183600001016     C                   KFLD                    kaAnsp                         -spedizione
183700001016     C                   KFLD                    kaAtrc                         -tipo riferimento
183800000921     C*--------------------
183900001016     C* CALCOLA LA DATA CORRENTE
184000000921     C*--------------------
184100000921     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
184200001003     C                   MOVE      WN14          WN6               6 0          *ORA (6)
184300000921     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
184400001120     C                   Z-ADD     WN8           G08DAT
184500000921     C                   Z-ADD     *ZEROS        G08INV
184600000921     C                   MOVEL     '0'           G08ERR
184700000921     C                   CALL      'XSRDA8'
184800000921     C                   PARM                    WLBDA8
184900000921     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
185000001120     C*--------------------
185100001120     C* SE PRESENTE INVERTE LA DATA DI RIFERIMENTO
185200001120     C*--------------------
185300001120     C                   if        PARDRI > *zeros
185400001120     C                   z-add     *zeros        G08DAT
185500001120     C                   z-add     PARDRI        G08INV
185600001120     C                   movel     '3'           G08ERR
185700001120     C                   CALL      'XSRDA8'
185800001120     C                   PARM                    WLBDA8
185900001120     C                   z-add     G08DAT        depdat                         *data di riferimento
186000001120     C                   endif
186100000000     C*--------------------
186200000000     C* CAMPI DI CONTROLLO
186300000000     C*--------------------
186400000000     C                   MOVEL     '0'           FINL00            1
186500000000     C                   Z-ADD     1             CODUT             1 0
186600001109     C*
186700001109     C                   SETON                                        01
186800000000     C*
186900000000     C* CARICA ORGANIGRAMMA
187000000000     C                   EXSR      CARORG
187100000000     C*
187200000000     C* CARICA TIPI BOLLA
187300000000     C                   EXSR      CARTBL
187400000000     C*
187500000000     C                   ENDSR
187600000914
187700000920** CMD - COMANDI CL
187800001016CLRPFM FILE(WFASS00F)
187900000920
