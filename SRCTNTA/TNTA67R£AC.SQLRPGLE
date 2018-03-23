000100020328     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200940712     H* TNTA67R  *---------------------------------------------------*
000300920529     H*               GESTIONE TARIFFE PARTICOLARI
000400920529     H*--------------------------------------------------------------*
000500940712     FTNTA67D   CF   E             WORKSTN
000600940913     F                                     SFILE(TA67S02:NRR)
000700940913     F                                     SFILE(TA67S07:NR2)
000800000829     F                                     SFILE(TA67S09:NR9)
000900061011     FTITPT01L  UF A E           K DISK    usropn
001000061011     ftiopt01l  uf a e           k disk    usropn
001100061011     f                                     rename(titpt000:tiopt000)
001200061011     FTITPD01L  UF A E           K DISK    usropn
001300920610     F                                     INFDS(TAR)
001400061011     ftiopd01l  uf a e           k disk    usropn
001500061011     F                                     infds(taro)
001600061011     f                                     rename(titpd000:tiopd000)
001700140127     fTIACB01L  if   e           k disk
001800920529     FTABEL00F  IF   E           K DISK
001900090916     FTivis05l  IF   E           K DISK    usropn
002000060306     fcnclp00f  if   e           k disk    usropn
002100060306     ftfclp00f  if   e           k disk    rename (cnclp000:tfclp000) usropn
002200950112     D*
002300950119     D MS1             S              1    DIM(79) CTDATA PERRCD(79)
002400950119     D MS2             S              1    DIM(79) CTDATA PERRCD(79)
002500060705     D MSG             S             79    DIM(20) CTDATA PERRCD(1)
002600950112     D* SCHIERE RELATIVE ALLA TABELLA "TR"
002700990614     D WTT             S              2    DIM(50)
002800920609     D TTR             S              1    DIM(50)
002900980428     D DTR             S             10    DIM(50)
003000941110     D TTP             S              1    DIM(50)
003100950119     D TSP             S              1    DIM(50)
003200980429     D ARR             S              1    DIM(50)
003300980429     D RPV             S              1    DIM(50)
003400950112     D* SCHIERE RELATIVE ALLA TABELLA "15"
003500950112     D NAZ             S              3    DIM(20)
003600080609      * schiera filiali abilitate al lasciato avviso
003700080609     d sklav           s              3  0 dim(999)
003800080609
003900050531     D TParEli         S              2    DIM(20)
004000050531     D TParDat         S              8  0 DIM(20)
004100021209
004200030715     d MANftc          s              3    dim(150)
004300030715     d W_manftc        s              3
004400030715     d WW              s              3  0 inz
004500051012     d Kscfil          s              3  0
004600080609
004700080609     d xx              s              3  0
004800060801
004900060801     D Wdataeur        S               D   DATFMT(*eur)
005000060801     D Wdataoggi       S               D   DATFMT(*eur)
005100060802     d Datamax         s              8  0
005200030715
005300021209     d Wfqt            s                   Like(§1pFqt)
005400050531     d
005500050531     d wdatadmy        s               d   datfmt(*dmy)
005600050704
005700050704     d savcts          s                   like(tpdcts)
005800061011     d codut           s              1  0 Inz(1)
005900060306     d kcc             s                   like(clpkcc) inz(151)
006000060306     d ksc             s                   like(clpksc)
006100021209
006200060802     d W_vi2dpb        s                   like(vi2dpb)
006300120320
006400120320       // -?Flag booleani
006500120320     d wFine           s               n   inz(*off)
006600130125     d wBlocca         s               n   inz(*off)
006700060802
006800941020     D*
006900920612     D PARAM5          DS
007000941111     D* CODICE CLIENTE/NUMERO VISITA
007100920612     D  VIDCLI                 2      8  0
007200941111     D* RAGIONE SOCIALE
007300920612     D  RAGDES                 9     56
007400941111     D* PROGRESSIVO TARIFFA
007500920612     D  VIDPRG                57     59  0
007600941111     D* CODICE TARIFFA
007700920612     D  VIDCTR                60     62  0
007800941111     D* DECODIFICA CODICE TARIFFA
007900920612     D  VIDDCF                63     72
008000941111     D* DESCRIZIONE TARIFFA
008100920612     D  VIDDCV                73     87
008200941111     D* TESTATA PROGRAMMA
008300920612     D  VIDTES                88    121
008400941111     D* FLG = " " ---> TARIFFE     FLG <> " " ---> OFFERTE
008500920619     D  FLG                  122    122
008600941111     D* FLG2   --> SE RESTITUITO PIENO INDICA CHE E' STATO PREMUTO CMD3
008700921117     D  FLG2                 123    123
008800980309     D* FLAG ITALIA/ESTERO
008900980309     D  PARFIE               125    125
009000980429     D* FLAG TARIFFA IMPORT
009100980429     D  IMPORT               126    126
009200000420     D* FLAG CLIENTE POSTA
009300000420     D  WRKPT                127    127
009400990824     D* CODICE DELLA  DIVISA
009500990824     D  VICDIV               128    130
009600990622     D* FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
009700990824     D  FLGDEC               131    131
009800000915     D* DATA DECORRENZA TARIFFA
009900000915     D  PARDDT               132    139  0
010000000915     D* TIPO SEWRVIZIO BOLLA
010100000915     D  PARTSP               140    140
010200020201     D* Flag Network
010300020201     D  FLGNET               141    141
010400050531     D* DATA scadenza TaRIFFA alfanumerica
010500050531     D  PARDST               142    149
010600060306      * flag rinuncia AC Base
010700060306     d  parfmp               150    150
010800090916      * flag per richiamo da trattativa = 'T'
010900090916     d  partrat              151    151
011000941111     D*
011100920616     D                 DS
011200940712     D  VI2SGL                 1     13  3
011300941110     D  VI2ITR                14     24  3
011400990614     D  VI2MIN                25     35  3
011500990614     D  VI2MAX                36     46  3
011600990614     D  RECORD                 1     46
011700990614     D  VI2AIN                47     47
011800990614     D  VI2REC                14     47
011900921126     D                 DS
012000941110     D  SAVITR                 1     11  3
012100990614     D  SAVMIN                12     22  3
012200990614     D  SAVMAX                23     33  3
012300990614     D  SAVAIN                34     34
012400990614     D  SAVREC                 1     34
012500990615     D WLBDAT          DS
012600060802     D  G08DAT                 1      8  0
012700060802     D  G08INV                 9     16  0
012800060802     D  G08ERR                17     17
012900060802     D  G08TGI                18     22  0
013000920612     D KPJBA         E DS
013100920610     D TAR             DS
013200920610     D  TPDNRR               397    400B 0
013300061011     d taro            ds
013400061011     d  tponrr               397    400B 0
013500990614     D TPTDS         E DS                  EXTNAME(TITPT00F)
013600921215     D DS1P          E DS
013700020201     D DSSP          E DS
013800990907     D DGEI          E DS
013900920612     D DSCT          E DS
014000940913     D DSTR          E DS
014100950112     D DS15          E DS
014200060705     d dfab          e ds
014300990217     D DSBS02        E DS                  EXTNAME(TIBS02DS)
014400030718     D TNTA04DS      E DS
014500061011
014600061011     d Azuteds       e ds                  extname(Azute00f)
014700061011     d dDatiute      e ds
014800061011     d Tibs34ds      e ds
014900130125     d dUTE01        e ds
015000061011
015100061011     d Tnta25ds      e ds
015200061011     d Tnta25tad     e ds                  inz
015300061011     d Tnta25tgc     e ds                  inz
015400061011     d Tnta25tpd     e ds                  inz
015500061011     d Tnta25tpt     e ds                  inz
015600080609
015700080609     d dlav          e ds
015800080609     d tntbeds       e ds                  extname(tntbe00f)
015900120320
016000120320       // -?ds dati passati da TNSB48R - Manca Tariffa
016100120320     d TNSB48ds      e ds
016200120320     d Parm48          s                   like(tnsb48ds)
016300120713
016400120713       // -?Passaggio dati a TRTB09R - Ricerca codici tassazione
016500120713     d TRTB09DS      e ds
016600090916
016700090916     itivis000
016800090916     i              visesi                      wisesi
016900090916
017000920529     C*****************************************************************
017100920529     C* RIEPILOGO INDICATORI
017200920529     C*****************************************************************
017300920609     C* 01    - IMMISSIONE
017400920609     C* 02    - VARIAZIONE
017500920618     C* 03    - USATO IN VARIAZIONE PER POSIZIONARMI SU CODICE TASSAZ.
017600920618     C* 04    - USATO IN VARIAZIONE PER POSIZIONARMI SU IMPORTO TARIFFA
017700920609     C* 05    - USATO IN VARIAZIONE PER PROTEGGERE I CAMPI
017800920609     C* 06    - TIPO CONSEGNA = ISOLE MINORI
017900920619     C* 07    - PREMUTO CMD6-AGGIORNAMENTO
018000920624     C* 08    - ROLLUP
018100920618     C* 09    - PREMUTO CMD16-ANNULLAMENTO
018200920707     C* 10    - USATO IN INTERROGAZIONE PER PROTEGGERE I CAMPI
018300941111     C* 11    - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
018400920619     C* 12    - PROVENGO DA OFFERTA
018500950119     C* 13    - INDICATORE DI EMISSIONE CAMPO MESSAGGIO DI ERRORE
018600941110     C* 14    - INDICA TIPO TARIFFA/TIPO PAGAMENTO A VALORE
018700920616     C* 15/19 - TASTI DI DUPLICA
018800920707     C* 20    - LOKUP
018900981016     C* 21    - ON  PER IL TIPO CONSEGNA  RCV
019000981016     C*         PROTEGGO IL CAMPO TIPO VALORE.DEVE ESSERE SEMPRE A KG
019100920625     C* 22    - USATO IN ASSICURAZIONE X CONTO PER PROTEGGERE CAMPI
019200920625     C*         DEL FORMATO
019300941111     C* 23    - TASTO DI DUPLICA
019400990217     C* 24    - TARIFFE PARTIC. ASSICURAZIONE X CONTO
019500990217     C* 25    - TAIRFFE PARTIC. RCV
019600920827     C* 26    - USATO X CONDIZIONARE L'EMISSIONE DEGLI ARROTONDAMENTI
019700941110     C* 27    - USATO X CONDIZIONARE L'EMISSIONE DEL   RAPPORTO P/VOL.
019800941111     C* 28    - VISUALIZZO CODICE APPLICAZIONE
019900921007     C* 29    - USATO PER AZZERARE I CAMPI DEL SFLCONTROL NEL CASO IN
020000941110     C*         CUI VENGA VARIATO IL TIPO TARIFFA/TIPO PAGAMENTO
020100920609     C* 30/34 - DI COMODO
020200920609     C* 35/37 - PULIZIA SUBFILE
020300920609     C* 36    - SFLNXTCHG
020400941116     C* 38    - VISUALIZZO IL MINIMO  TARIFFA
020500941116     C* 39    - VISUALIZZO IL MASSIMO TARIFFA
020600941111     C* 40/42 - ERRORE
020700980514     C* 43    - VISUALIZZO IL FLAG MANDATO ASSICURATIVO FITTIZIO
020800990217     C*         SOLO EPR "C" -ASS/C
020900000918     C* 44    - SI TRATTA DI TARUFFA DI CARTELLO PROTEGGO LA COPIA TARIFFE DI
021000000918     C*         CARTELLO
021100941111     C* 45/51 - ERRORE
021200980429     C* 52/59 - ERRORE
021300921126     C* 60    - ON  SIGNIFICA CHE DEVO REGISTRARE IL RECORD
021400941111     C* 61    - USATO PER EMETTERE IL MESSAGGIO DEL CMD14 DI FORZATURA
021500031003     C* 62    - ON SIGNIFICA CHE NON C'E' DETTAGLIO TARIFFE PARTICOLARI
021600990622     C* 63/65 - ERRORE
021700990622     C* 66/73 - ERRORE
021800950119     C* 74    - POSIZIONAMENTO CURSORE SUL CODICE TASSAZIONE PER
021900950119     C*         EMISSIONE CAMPO MESSAGGIO
022000060303     C* 76/77 - ERRORE VI2CTS
022100060303      * 78    - VARIA "d" AC BASE
022200060306      * 79    - Posizionamento cursore
022300000829     C* 80/81 - PULIZIA SUBFILE
022400060802      * 82/85 - Tariffa Particolare "f" Supplemento carburante
022500080922      * 88    - Tariffa particolare "f" deve avere scaglione >= 999 OBSOLETO
022600090916      * 89    - Trattativa
022700120320      * 97    - Pgm richiamato x manca tariffa TNSB48
022800920611     C* 98    - DI COMODO
022900920609     C* 99    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
023000920529     C*****************************************************************
023100920529     C     *ENTRY        PLIST
023200920529     C                   PARM                    KPJBA
023300030715     C                   PARM                    manftc
023400120320     c                   parm                    Parm48
023500920615     C                   MOVEL     KPJBU         PARAM5
023600080609
023700080609     c/exec sql
023800080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
023900080609     c/end-exec
024000061011
024100061011     c     *dtaara       define    §azute        azuteds
024200061011     c     *dtaara       define    §datiute      ddatiute
024300061011     c                   in(E)     *dtaara
024400061011     c                   If        %error  or RSUT = *blanks
024500061011     c                   Clear                   Tibs34ds
024600061011     c                   Call      'TIBS34R'
024700061011     c                   Parm                    Tibs34ds
024800061011     c                   In        *dtaara
024900061011     c                   EndIf
025000120320
025100120320
025200120320      /free
025300120320       //?controllo se passati tutti i parametri
025400120320         IF  %parms < 3;
025500120320           clear tnsb48ds;
025600120320           *in97 = *off;
025700120320         ELSE;
025800120320           clear tnsb48ds;
025900120320           tnsb48ds = parm48;
026000120320           *in97 = *on;
026100120320         ENDIF;
026200120320
026300120320         clear OSB48err;
026400120320         clear OSB48msg;
026500120320      /end-free
026600120320
026700120320
026800990615     C* PRENDO LA DATA DEL GIORNO DA TIME
026900990615     C                   TIME                    W0140            14 0
027000990615     C                   MOVE      W0140         UDATE8            8 0
027100990615     C*
027200990615     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
027300990615     C*
027400060802     C                   Z-ADD     UDATE8        G08DAT
027500060802     C                   MOVEL     *BLANK        G08ERR
027600990615     C                   CALL      'XSRDA8'
027700990615     C                   PARM                    WLBDAT
027800990615     C* UDATE A 8 IN AAAA/MM/GG
027900060802     C                   Z-ADD     G08INV        DATEU8            8 0
028000060801     C     *iso          movel     dateu8        Wdataoggi
028100060801      * calcolo la data massima di controllo per supplemento carburante
028200060803     c     Wdataoggi     adddur    06:*m         Wdataeur
028300060802     c     *iso          movel     Wdataeur      Datamax
028400090916
028500090916      * controllo se richiamato da trattativa
028600090916     c                   if        partrat = 'T'
028700090916     c                   eval      *in89 = *on
028800090916     c                   else
028900090916     c                   eval      *in89 = *off
029000090916     c                   endif
029100920529     C*---------------------------------------------------------------*
029200990614     C* ACCESSO TITPT01L/TITPD01L
029300920612     C     KTPT          KLIST
029400920612     C                   KFLD                    VIDCLI
029500920612     C                   KFLD                    VIDCTR
029600920612     C                   KFLD                    VIDPRG
029700920612     C                   KFLD                    VIDFTC
029800000901     C* ACCESSO A TPT/TPD DA SFL COPIA DA CARTELLO
029900000901     C     KTPT9         KLIST
030000000901     C                   KFLD                    VIDCLI
030100000901     C                   KFLD                    VIDCTR
030200000901     C                   KFLD                    VIDPRG
030300000901     C                   KFLD                    V09FTC
030400990614     C* ACCESSO TITPD01L
030500920612     C     KTPD          KLIST
030600920612     C                   KFLD                    VIDCLI
030700920612     C                   KFLD                    VIDCTR
030800920612     C                   KFLD                    VIDPRG
030900920612     C                   KFLD                    VIDFTC
031000920612     C                   KFLD                    VI2ORP
031100940712     C                   KFLD                    VI2SGL
031200050704      *
031300050704     c     ktpd01        klist
031400050704     c                   kfld                    vidcli
031500050704     c                   kfld                    vidctr
031600050704     c                   kfld                    vidprg
031700050704     c                   kfld                    vidftc
031800050704     c                   kfld                    vi2orp
031900940712     C* ACCESSO TABEL00F
032000920608     C     KTAB          KLIST
032100920608     C                   KFLD                    CODUT
032200940712     C                   KFLD                    COD
032300940712     C                   KFLD                    KEY
032400920608     C     KTAB2         KLIST
032500920608     C                   KFLD                    CODUT
032600920608     C                   KFLD                    COD
032700060306
032800060306      * cnclp00f/tfclp00f
032900060306     c     kclp          klist
033000060306     c                   kfld                    codut
033100060306     c                   kfld                    kcc
033200060306     c                   kfld                    ksc
033300140127
033400140127      * TIACB01L
033500140127     c     kACB          klist
033600140127     c                   kfld                    CLPclv
033700140127     c                   kfld                    dateu8
033800140127
033900940712     C***
034000940712     C** DEFINIZIONE CAMPI
034100940712     C***
034200940712     C     *LIKE         DEFINE    TBLCOD        COD
034300940712     C     *LIKE         DEFINE    TBLKEY        KEY
034400941109     C     *LIKE         DEFINE    §1PCTS        WCTS
034500941123     C     *LIKE         DEFINE    §1PCTS        WCTE
034600941109     C     *LIKE         DEFINE    §1PPTV        WPTV
034700940712     C* CAMPI DI DUPLICA
034800940712     C     *LIKE         DEFINE    TPDCTS        DUPCTS
034900940712     C     *LIKE         DEFINE    TPDSGL        DUPSGL
035000941110     C     *LIKE         DEFINE    TPDITR        DUPITR
035100940712     C     *LIKE         DEFINE    TPDMIN        DUPMIN
035200940712     C     *LIKE         DEFINE    TPDMAX        DUPMAX
035300940712     C     *LIKE         DEFINE    TPDAIN        DUPAIN
035400940712     C*
035500920529     C*---------------------------------------------------------------*
035600930223     C***** REPERISCO IL LIMITE RISARCIBILE STABILITO PER LEGGE  *****
035700990907     C*****   DALLA TABELLA GEN PASSANDOGLI LA MONETA            *****
035800990907      *
035900990907     C                   CLEAR                   DSBS02
036000990907     C                   CLEAR                   DGEI
036100990907     C                   MOVEL     'C'           T02MOD
036200990907     C                   MOVEL     KNSIF         T02SIF
036300990907     C                   MOVEL     'GEI'         T02COD
036400990907     C                   MOVEL     VICDIV        T02KE1
036500990907      *
036600990907     C                   CALL      'TIBS02R'
036700990907     C                   PARM                    KPJBA
036800990907     C                   PARM                    DSBS02
036900990907      *
037000990907     C     T02ERR        IFEQ      *BLANKS
037100990907     C                   MOVEL     T02UNI        DGEI
037200990907     C                   ENDIF
037300920609     C*
037400980317     C* CARICO TABELLA CODICI NAZIONE
037500980317     C                   EXSR      CARNAZ
037600080609
037700080609      * carico tabella filiali attive al lasciato avviso
037800080609     c                   exsr      carlav
037900920619     C*
038000920619     C* CONTROLLO SE PROVENGO DA OFFERTA
038100920619     C     FLG           IFNE      ' '
038200920619     C                   Z-ADD     VIDCLI        VIDNRV
038300090916     C                   Z-ADD     VIDCLI        VIDNRVT
038400920619     C                   SETON                                        12
038500110103     c**n89              open      tnvis05l
038600090916     c   89              open      tivis05l
038700110103     c**n89vidnrv        chain     tnvis05l
038800090916     c   89vidnrv        chain     tivis05l
038900090916     c                   If        %found
039000051012     c                   movel     viscmm        kscfil
039100051012     c                   else
039200051012     c                   clear                   kscfil
039300051012     c                   endif
039400110103     c**n89              close     tnvis05l
039500090916     c   89              close     tivis05l
039600060306     c                   open      tfclp00f
039700111122     c                   open      cnclp00f
039800061011
039900061011     c                   open      tiopt01l
040000061011     c                   open      tiopd01l
040100061011
040200051012     c                   else
040300051012      * no offerta
040400051012     c                   movel     vidcli        kscfil
040500060306     c                   open      cnclp00f
040600061011     c                   open      titpt01l
040700061011     c                   open      titpd01l
040800061011     c                   setoff                                       12
040900920619     C                   END
041000990217     C**
041100040429      * verifico codice cliente se inizia con 88888 si tratta di CARTELLO
041200040429     c                   movel     vidcli        w0050             5 0
041300040429     c                   if        w0050 = 88888
041400040429     c                   seton                                        44
041500040429     c                   endif
041600060306      * se non è tariffa di cartello recupero il cod.importanza del CLIENTE
041700060306      * o della VISITA
041800060306     c                   if        not *in44
041900060306     c                   move      vidcli        ksc
042000060306     c  n12kclp          chain     cnclp00f
042100060306     c   12kclp          chain     tfclp00f
042200111122     c                   If        *in12 and not %found(tfclp00f)
042300111122      * se non trovata l'anagrafica provvisoria aggancio l'anagrafica del client
042400111122      * e della trattativa
042500111122     c                   move      visksc        Ksc
042600111122     c     kclp          chain     cnclp00f
042700111122     c                   endif
042800111121     c                   If        clpclv = ' '
042900111121     c                   eval      clpclv = 'C'
043000111121     c                   Endif
043100060306     c                   endif
043200130125
043300130125       //?Imposto flag x bloccare le tariffe particolari in manutenzione
043400130125       //?ma solo TARIFFE e NO CARTELLO
043500130125     c                   eval      dUTE01 = UTEfaf
043600130125     c                   IF        not *in12 and not *in44
043700130125     c                             and §UTEmodtar = *blanks
043800130125     c                   eval      wBlocca = *on
043900130125     c                   ENDIF
044000000829     C*
044100000829     C     CARTAG        TAG
044200921126     C*
044300920529     C* CARICO SUBFILE
044400920529     C                   EXSR      CARICA
044500920609     C* SE NON CARICA NIENTE ESCO
044600920529     C     NRR           IFEQ      0
044700920529     C                   GOTO      FINE
044800920529     C                   END
044900920529     C*
045000920529     C     INIZIO        TAG
045100940913     C                   WRITE     TA67T01
045200940913     C                   WRITE     TA67Z03
045300940913     C                   EXFMT     TA67C02
045400050531     c
045500050531     c                   setoff                                       4013
045600920529     C*
045700921117     C** CMD3  - FINE LAVORO
045800921117     C   KC              MOVEL     'A'           FLG2
045900921118     C   KC
046000921117     C** CMD12 - RITORNO
046100921117     COR KL              GOTO      FINE
046200030718     C** CMD07 - INTERROGAZIONE STORICO VARIAZIONI
046300030718     c                   if        *inkg
046400030718     c                   clear                   tnta04ds
046500030718     c                   eval      i04ksc = vidcli
046600030718     c                   eval      i04ctr = vidctr
046700030718     c                   eval      i04prg = vidprg
046800030718     c                   eval      i04rag = ragdes
046900030718     c                   eval      i04dct = viddcf
047000030718     c                   eval      i04dcv = viddcv
047100030718     c                   eval      i04div = vicdiv
047200030718     c                   eval      i04ndf = flgnet
047300030718      *
047400030718     c                   call      'TNTA04R'
047500030718     c                   parm                    tnta04ds
047600030718      *
047700030718     c                   goto      inizio
047800030718      *
047900030718     c                   endif
048000120320       //?F1 - dati manca tariffa
048100120320     c                   IF        *inka
048200120320     c                   exsr      gesw01
048300120320     c                   goto      inizio
048400120320     c                   ENDIF
048500030718     C*
048600000829     C** CMD13 - COPIA TARIFFE PARTICOLARI DA CARTELLO
048700000829     C     *INKM         IFEQ      *ON
048800000829     C                   EXSR      COPCAR
048900000829     C                   GOTO      CARTAG
049000000829     C                   ENDIF
049100000829     C**
049200920529     C* CONTROLLO DATI SUBFILE
049300920529     C                   SETOFF                                       31
049400940913     C                   READC     TA67S02                                31
049500921118     C   31              GOTO      FINE
049600050531    1C     *IN31         DOWEQ     '0'
049700980317     C*
049800980317     C                   SETOFF                                       0709
049900050531     c                   z-add     1             rec
050000920708     C*
050100050531    2C     VIDSCE        IFEQ      '1'
050200050531     c                   clear                   wfqt
050300050531     c* se tariffa non essitente, non la faccio inserire se in scadenza,
050400050531     c*  da eliminare
050500050531    3c                   if        vidflg=' ' and viheli>*zeros and
050600050531     c                             pardst>=viheli
050700050531     c                   eval      vidmsg=msg(5)
050800050531     c                   eval      %subst(vidmsg:42:2)=vidftc
050900050531     c                   movel     viheli        wdata8            8 0
051000050531     c     *iso          move      wdata8        wdatadmy
051100050531     c                   move      wdatadmy      wdata6            6 0
051200050531     c                   eval      %subst(vidmsg:68:8) =(%editw(wdata6:'  /  / -
051300050531     c                              '))
051400050531     c                   seton                                        13
051500050531    3c                   endif
051600060306
051700060306      * se immissione della tariffa particolare "d " AC BASE la tariffa non deve
051800060306      * avere il flag di rinuncia AC BASE
051900060306     c                   if        not *in13 and vidflg = *blanks and
052000060306     c                             vidftc = 'd ' and parfmp = 'S'
052100060306     c                   eval      vidmsg = msg(8)
052200060306     c                   eval      *in13 = *on
052300060306     c                   endif
052400060306
052500060306      * se immissione della tariffa particolare "d " AC BASE la tariffa non deve
052600060306      * avere una tariffa particolare AC PLUS reale con applicazione completa
052700060306     c                   if        not *in13 and vidflg = *blanks and
052800060306     c                             vidftc = 'd '
052900060306     c                   movel     'C '          v09ftc
053000061011     c  n12ktpt9         chain(n)  titpt01l
053100061011     c   12ktpt9         chain(n)  tiopt01l
053200061011     c                   if        %found and tptatb = *blanks and
053300060306     c                             tpttma = *blanks and tptapl = *blanks
053400060306     c                   eval      vidmsg = msg(9)
053500060306     c                   eval      *in13 = *on
053600060306     c                   endif
053700060306     c                   endif
053800130104
053900130104      * se scelta della tariffa particolare "f " Fuel Surcharge errore
054000130104      * in caso di manutenzione e tariffa, la cartello è libera
054100130128     c*******            IF        not *in13 and vidflg <> *blanks and
054200130128     c*******                      not *in12 and not *in44 and vidftc = 'f '
054300130128     c*******            eval      vidmsg = 'Impossibile manutenzionare la -
054400130128     c*******                      Tariffa Particolare Fuel Surcharge'
054500130128     c*******            eval      *in13 = *on
054600130128     c*******            ENDIF
054700970401     C**
054800050531    3c                   if        not *in13
054900920529     C                   MOVEL     VIDFTC        VI2FTC
055000920612     C                   MOVEL     VIDDES        VI2DTC
055100980317     C* PULIZIA DELLE SCHIERE CODICI TARIFFE PER LA SINGOLA TARIFFA PARTICOLARE
055200980317     C                   MOVEL     *BLANKS       TTR
055300980317     C                   MOVEL     *BLANKS       DTR
055400980317     C                   MOVEL     *BLANKS       TTP
055500980317     C                   MOVEL     *BLANKS       TSP
055600980317     C                   MOVEL     *BLANKS       WTT
055700980429     C                   MOVEL     *BLANKS       ARR
055800980429     C                   MOVEL     *BLANKS       RPV
055900920831     C*
056000941020     C* CARICO I TIPI TARIFFA VALIDI PER LA CONS.PARTICOL. SELEZIONATA
056100941020     C                   MOVEL     '1P'          COD
056200941020     C                   MOVEL     *BLANKS       KEY
056300941020     C                   MOVEL     VIDFTC        KEY
056400941020     C     KTAB          CHAIN     TABEL                              34
056500050531    4C     *IN34         IFEQ      *OFF
056600941109     C* UTT --> TIPI TARIFFA VALIDI PER LA CONSEGNA SELEZIONATA        A
056700980428     C                   MOVEL     TBLUNI        DS1P
056800130125
056900130125       //?Se non posso manutenzionare le tariffe ed è una tariffa
057000130125       //?particolare NON sbloccata errore
057100130125     c                   IF        wBlocca and §1Pmodtp = *blanks
057200130125     c                   eval      vidmsg = 'Impossibile gestire la Tariffa -
057300130125     c                             Particolare selezionata'
057400130125     c                   eval      *in13 = *on
057500130125     c                   goto      nomantp
057600130125     c                   ENDIF
057700130125
057800980428     C                   MOVEL     §1PTT0        WTT(1)
057900980428     C                   MOVEL     §1PTT1        WTT(2)
058000980428     C                   MOVEL     §1PTT2        WTT(3)
058100980428     C                   MOVEL     §1PTT3        WTT(4)
058200980428     C                   MOVEL     §1PTT4        WTT(5)
058300980428     C                   MOVEL     §1PTT5        WTT(6)
058400980428     C                   MOVEL     §1PTT6        WTT(7)
058500980428     C                   MOVEL     §1PTT7        WTT(8)
058600980428     C                   MOVEL     §1PTT8        WTT(9)
058700980428     C                   MOVEL     §1PTT9        WTT(10)
058800980428     C                   MOVEL     §1PTTV        WTT(11)
058900980317     C*
059000980317     C* CARICO TABELLA CODICI TARIFFA
059100980317     C                   EXSR      CARTAB
059200941020     C*
059300941109     C* WCTS --> SE PIENO, IL DETTAGLIO DELLA TARIFFA PARTICOLARE DEVE
059400941109     C*          AVERE COME CODICE TASSAZ. IL CODICE CONTENUTO IN WCTS
059500050531    5C     PARFIE        IFEQ      'I'
059600941123     C* SE TARIFFA ITALIA DEVO PRENDERE COD.TASS. ITALIA
059700941109     C                   MOVEL     §1PCTS        WCTS
059800941123     C                   CLEAR                   WCTE
059900941109     C                   ELSE
060000050531    6C     PARFIE        IFEQ      'E'
060100941123     C* SE TARIFFA ESTERO DEVO PRENDERE COD.TASS. ESTERO
060200941109     C                   MOVEL     §1PCTE        WCTS
060300941123     C                   CLEAR                   WCTE
060400941109     C                   ELSE
060500941123     C* SI TRATTA DI TARIFFA DI CARTELLO QUINDI POSSONO ESISTERE
060600941123     C*   ENTRAMBI I CODICI TASSAZIONE ESPRESSI IN TABELLA
060700941123     C                   MOVEL     §1PCTS        WCTS
060800941123     C                   MOVEL     §1PCTE        WCTE
060900050531    6C                   ENDIF
061000050531    5C                   ENDIF
061100941109     C*
061200941109     C* SALVO LA MASSIMA % PER TARIFFA A VALORE
061300941109     C                   MOVEL     §1PPTV        WPTV
061400941111     C* 11 ON  - VISUALIZZO IL VALORE MERCE E IL FLAG VALORE MERCE
061500941111     C     §1PVVM        COMP      'S'                                    11
061600941111     C* 28 ON  - VISUALIZZO IL TIPO APPLICAZIONE
061700941111     C     §1PVCA        COMP      'S'                                    28
061800941116     C* 38 ON  - VISUALIZZO IL MINIMO  TARIFFA
061900941116     C     §1PVMN        COMP      'S'                                    38
062000941116     C* 39 ON  - VISUALIZZO IL MASSIMO TARIFFA
062100941116     C     §1PVMX        COMP      'S'                                    39
062200980514     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO FITTIZIO
062300050531     C     VIDFTC        COMP      'C'                                    43
062400060802     C* 82 ON  - VISUALIZZO DATA riferimento prezzo base carburante
062500060802     C     §1pdrf        comp      'S'                                    82
062600060802      * Salvo il flag che mi permette di inserire una tariffa particolare
062700021209      * a qtà anche se la tariffa non è a qtà
062800021209     c                   Movel     §1pFqt        Wfqt
062900050531    4C                   ENDIF
063000941020     C*
063100941110     C                   EXSR      VIDEO2
063200050531    3c                   endif
063300130125
063400130125     c     nomantp       tag
063500920827     C*
063600940913     C     NRR           CHAIN     TA67S02                            31
063700920529     C                   MOVEL     ' '           VIDSCE
063800920708     C*
063900920619     C* 07 ON - PREMUTO  CMD6-AGGIORNAMENTO
064000920619     C   07              MOVEL     'S'           VIDFLG
064100990907     C   07              Z-ADD     UDATE8        VIDDUV
064200920708     C*
064300920618     C* 09 ON - PREMUTO  CMD16-ANNULLAMENTO
064400920708     C   09              MOVEL     ' '           VIDFLG
064500990907     C   09              CLEAR                   VIDDUV
064600050531     c* Se errore, mi posizione sulla tariffa particolare
064700050531     c                   if        *in13
064800050531     c                   seton                                        40
064900050531     c                   z-add     nrr           rec
065000050531     c                   endif
065100050531     c
065200940913     C                   UPDATE    TA67S02
065300050531    2C                   END
065400920708     C*
065500050531     c                   if        not *in13
065600940913     C                   READC     TA67S02                                31
065700050531     c                   else
065800050531     c                   seton                                        31
065900050531     c                   endif
066000050531     c
066100050531    1c                   ENDdo
066200921119     C                   GOTO      INIZIO
066300920529     C*
066400920529     C     FINE          TAG
066500921126     C                   MOVEL     PARAM5        KPJBU
066600120320
066700120320     c                   IF        *in97
066800120320     c                   eval      parm48 = tnsb48ds
066900120320     c                   ENDIF
067000120320
067100990917      * CHIUSURA PGM RICERCA BOLLE
067200990917     C                   CLEAR                   DSBS02
067300990917     C                   MOVEL     'C'           T02TLA
067400990917     C                   CALL      'TIBS02R'
067500990917     C                   PARM                    KPJBA
067600990917     C                   PARM                    DSBS02
067700921126     C*
067800920529     C                   SETON                                        LR
067900920609     C*
068000941110     C*--- CARICAMENTO TABELLE ---------------------------------------*
068100920609     C     CARTAB        BEGSR
068200941110     C****  CODICI TARIFFA E DESCRIZIONI  ****
068300920609     C                   MOVEL     'TR'          COD
068400920609     C                   Z-ADD     1             X                 3 0
068500980317     C* CARICO I TIPI TARIFFE CHE VENGONO GESTITE DALLA SINGOLA TARIFFA
068600980317     C* PARTICOLARE
068700990614    1C     WTT(X)        DOWNE     '  '
068800980317     C                   MOVEL     WTT(X)        KEY
068900980317     C     KTAB          CHAIN     TABEL                              30
069000980317    2C     *IN30         IFEQ      '0'
069100980317     C     TBLFLG        ANDEQ     *BLANKS
069200940921     C                   MOVEL     TBLUNI        DSTR
069300940913     C*
069400980317     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE PARTICOLARI
069500980317    3C     §TRUPA        IFEQ      'S'
069600920612     C                   MOVEL     TBLKEY        TTR(X)
069700941110     C                   MOVEL     §TRDES        DTR(X)
069800950112     C* IMPORTO TARIFFA  IN % O £ ("S"=TARIFFA IN % - " "=TARIFFA IN £)
069900941110     C                   MOVEL     §TRTAP        TTP(X)
070000950119     C* SCAGLIONE CON DECIMALI O SENZA ("S"=CON DECIM." "=SENZA DECIM.)
070100950112     C                   MOVEL     §TRSAP        TSP(X)
070200980429     C* ARROTONDAMENTI
070300980429     C                   MOVEL     §TRARR        ARR(X)
070400980429     C* RAPPORTO PESO VOLUME
070500980429     C                   MOVEL     §TRRPV        RPV(X)
070600980317    3C                   END
070700980316     C*
070800980317    2C                   END
070900980317     C*
071000980317     C                   ADD       1             X
071100980317    1C                   ENDDO
071200980317     C*
071300980317     C                   ENDSR
071400980317     C*-------------------------------------------------------------------------
071500980317     C     CARNAZ        BEGSR
071600950112     C****  CODICI NAZIONE  ****
071700950112     C                   Z-ADD     1             X
071800950112     C                   MOVEL     '15'          COD
071900950112     C     KTAB2         SETLL     TABEL
072000950112     C     KTAB2         READE     TABEL                                  32
072100950112     C*
072200950112     C     *IN32         DOWEQ     *OFF
072300950112     C     TBLFLG        IFEQ      ' '
072400950112     C                   MOVEL     TBLUNI        DS15
072500950112     C     §15ITA        IFEQ      'I'
072600950112     C                   MOVEL     TBLKEY        NAZ(X)
072700950112     C                   ADD       1             X
072800950112     C                   ENDIF
072900950112     C                   ENDIF
073000950112     C*
073100950112     C     KTAB2         READE     TABEL                                  32
073200950112     C                   ENDDO
073300050531     c*
073400950112     C*
073500920609     C                   ENDSR
073600080609
073700080609      *------------------------------------------------------------------------*
073800080609      * Carico filiali abilitate al lasciato avviso
073900080609      *------------------------------------------------------------------------*
074000080609     c     carlav        begsr
074100080609
074200080609     c                   clear                   sklav
074300080609     c                   clear                   xx
074400080609
074500080609     c/exec sql
074600080609     c+ declare lav cursor for select tntbe01l.*
074700080609     c+ from tntbe01l
074800080609     c+ where tbecod='LAV' and tbeatb = ''
074900080609     c/end-exec
075000080609
075100080609     c/exec sql
075200080609     c+ open lav
075300080609     c/end-exec
075400080609
075500080609     c                   do        *hival
075600080609
075700080609     c/exec sql
075800080609     c+ fetch next from lav into :tntbeds
075900080609     c/end-exec
076000080609
076100080609     c                   select
076200080609     c                   when      sqlcod = 100
076300080609     c                   leave
076400080609     c                   when      sqlcod < 0
076500080609     c                   other
076600080609     c                   eval      dlav = tbeuni
076700080609      * carico la schiera della filiali abilitate
076800080616     c                   if        d§lavtar > dateu8
076900080609     c                   iter
077000080609     c                   endif
077100080609     c                   eval      xx = xx + 1
077200080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
077300080609
077400080609     c                   endsl
077500080609
077600080609     c                   enddo
077700080609
077800080609     c/exec sql
077900080609     c+ close lav
078000080609     c/end-exec
078100080609
078200080609
078300080609     c                   endsr
078400920609     C*
078500920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
078600920529     C     CARICA        BEGSR
078700920529     C                   SETOFF                                       3031
078800920529     C* PULIZIA SUBFILE
078900920529     C                   EXSR      PULSF
079000920529     C*
079100920529     C                   Z-ADD     0             NRR               4 0
079200920529     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
079300920529     C                   MOVEL     '1P'          COD
079400920608     C     KTAB2         SETLL     TABEL
079500920608     C     KTAB2         READE     TABEL                                  30
079600970325    1C     *IN30         DOWEQ     '0'
079700990621     C* CONTROLLO VALIDITA' TARIFFA PARTICOLARE PER LA GESTIONE TARIFFE
079800970325    2C     TBLFLG        IFEQ      *BLANKS
079900920827     C                   MOVEL     TBLUNI        DS1P
080000020204      * FLAG DI COMODO PER TABELLA 'SP'
080100020204     C                   CLEAR                   FLAGSP
080200080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
080300080609      * per le filiali non abilitate, accetto cartello
080400051012     c                   If        tblkey = 'c       '  and
080500080609     c                             kscfil <> 888
080600080609     c                   if        %lookup(kscfil:sklav) = *zeros
080700080609     c                             and %lookup(999:sklav) = *zeros
080800051012     c                   goto      legtab
080900080609     c                   endif
081000051012     c                   endif
081100060303      * ESCLUDO la tariffa particolare "d" se tariffa di cartello
081200060303     c                   if        tblkey = 'd       '  and *in44
081300060303     c                   goto      legtab
081400060303     c                   endif
081500051012      *
081600020204      * Carico solo le tariffe particolari che hanno flag <> 'N'
081700020204      * Per tariffa con tipo servizio poste controllo flag §1pfpt
081800020201     C     PARTSP        IFEQ      'P'
081900020201     C     §1PFPT        ANDEQ     'N'
082000020201     C                   GOTO      LEGTAB
082100020201     C                   ENDIF
082200020201      * Per tariffa con Network DPD controllo flag §1pfdp
082300020201     C     FLGNET        IFEQ      'D'
082400020201     C     §1PFDP        ANDEQ     'N'
082500020201     C                   GOTO      LEGTAB
082600020201     C                   ENDIF
082700020201      * Per tariffa con Network FedEx controllo flag §1pffe
082800020201     C     FLGNET        IFEQ      'F'
082900020201     C     §1PFFE        ANDEQ     'N'
083000040428      * tariffa FEDEX abilitata solo per le tariffe di cartello e 44 spento
083100040428      * (44 acceso solo se sto modificando una tariffa di cartello)
083200040428     c     flgnet        oreq      'F'
083300040428     c     §1pffe        andeq     'C'
083400040428     c     *in44         andeq     *off
083500020201     C                   GOTO      LEGTAB
083600020201     C                   ENDIF
083700020201      * Per tariffa Italia controllo flag §1ptco
083800020201     C     PARFIE        IFEQ      'I'
083900020205     C     PARTSP        ANDNE     'P'
084000020201     C     §1PTCO        ANDEQ     'N'
084100020201     C                   GOTO      LEGTAB
084200020201     C                   ENDIF
084300020201      * Per tariffa Estero controllo flag §1pfie
084400020201     C     PARFIE        IFEQ      'E'
084500020205     C     FLGNET        ANDEQ     ' '
084600020201     C     §1PFIE        ANDEQ     'N'
084700020201     C                   GOTO      LEGTAB
084800020201     C                   ENDIF
084900020225      * Per tariffe di cartello verifico che non sia tipo servizio "P"
085000020225     C     PARFIE        IFEQ      ' '
085100020225     C     PARTSP        ANDNE     'P'
085200020225     C     §1PTCO        ANDEQ     'N'
085300020225     C                   GOTO      LEGTAB
085400020225     C                   ENDIF
085500020201      *
085600020201      * Utilizzabile in tariffa
085700000420     C**
085800970325    3C     §1PUTA        IFEQ      'S'
085900970325     C*
086000920529     C                   MOVEL     TBLKEY        VIDFTC
086100920529     C                   MOVEL     §1PDES        VIDDES
086200050531     C                   MOVEL     §1Peli        VIheli
086300020201      *
086400020201      * Per tariffa FedEx controllo se devo recupare la descrizione
086500020201      * dalla tabella SP
086600020201     C     FLGNET        IFEQ      'F'
086700020201     C     §1PDFE        ANDEQ     'S'
086800020204     C                   MOVEL     '1'           FLAGSP            1
086900020201     C                   CLEAR                   DSSP
087000020201     C                   MOVEL     'SP'          COD
087100020201     C                   MOVEL     *BLANKS       KEY
087200020201     C                   MOVEL     VIDFTC        KEY
087300020204     C     KTAB          CHAIN     TABEL                              34
087400020201     C     *IN34         IFEQ      *OFF
087500020201     C                   MOVEL     TBLUNI        DSSP
087600020201     C                   ENDIF
087700020201     C     §SPDFE        IFNE      *BLANKS
087800020201     C                   MOVEL     §SPDFE        VIDDES
087900020201     C                   ENDIF
088000020201     C                   ENDIF
088100020201      *
088200920529     C* CAMPO SELEZIONE
088300920529     C                   MOVEL     ' '           VIDSCE
088400920529     C*
088500061011     C  n12KTPT          CHAIN     TITPT000                           31
088600061011     c   12ktpt          chain     tiopt000                           31
088700990903     C                   Z-ADD     0             VIDDUV
088800970325    5C     *IN31         IFEQ      '0'
088900920529     C* CONTROLLO VALIDITA' TARIFFA
089000970325    6C     TPTATB        IFEQ      ' '
089100920529     C* TARIFFA PARTICOLARE GIA' ESISTENTE
089200920529     C                   MOVEL     'S'           VIDFLG
089300990903     C* VALORIZZO L'ULTIMA DATA VARIAZIONE
089400990903     C*
089500990903     C* GIRO DATA
089600990903     C*
089700060802     C                   Z-ADD     TPTDUV        G08INV
089800060802     C                   MOVEL     '3'           G08ERR
089900990903     C                   CALL      'XSRDA8'
090000990903     C                   PARM                    WLBDAT
090100990903     C*
090200060802     C                   Z-ADD     G08DAT        VIDDUV
090300990903     C*
090400920529     C                   ELSE
090500920529     C* TARIFFA ANNULLATA
090600920708     C                   MOVEL     ' '           VIDFLG
090700970325    6C                   END
090800920529     C*
090900970325   X5C                   ELSE
091000920529     C* TARIFFA PARTICOLARE NON ESISTENTE
091100920708     C                   MOVEL     ' '           VIDFLG
091200970325    5C                   END
091300920529     C*
091400990217     C* SE C'E' LA TARIFFA -->VISUALIZZO
091500130607    5C**** VIDFLG        IFEQ      'S'
091600990217     C* SE NON E' UNA TARIFFA CHE RICHIEDE L'ABILITAZIONE --> VISUAL.
091700990217     C*
091800920529     C                   ADD       1             NRR
091900940913     C                   WRITE     TA67S02
092000130607    5C*****              END
092100010920    3C                   END
092200010920    2C                   END
092300000420     C*
092400000420     C     LEGTAB        TAG
092500020204     C     FLAGSP        IFNE      *BLANKS
092600020201     C                   MOVEL     '1P'          COD
092700020204     C     KTAB          SETGT     TABEL
092800020204     C                   ENDIF
092900920611     C     KTAB2         READE     TABEL                                  30
093000970325    1C                   END
093100920529     C                   Z-ADD     1             REC
093200990621     C*
093300920529     C                   ENDSR
093400920529     C*
093500920529     C*--- PULIZIA SUBFILE -------------------------------------------*
093600920529     C     PULSF         BEGSR
093700920529     C                   SETON                                        35
093800940913     C                   WRITE     TA67C02
093900920529     C                   SETOFF                                       35
094000920529     C                   ENDSR
094100000829     C*---------------------------------------------------------------*
094200000829     C*    CARICO DATI IN SUBFILE COPIA TARIFFE DA CARTELLO           *
094300000829     C*---------------------------------------------------------------*
094400000829     C     COPCAR        BEGSR
094500000829     C                   SETOFF                                       8081
094600000829     C* PULIZIA SUBFILE
094700091104     c                   z-add     1             rec
094800000829     C                   SETON                                        80
094900000829     C                   WRITE     TA67C09
095000000829     C                   SETOFF                                       80
095100000829     C*
095200000829     C                   Z-ADD     0             NR9               4 0
095300000829     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
095400000829     C                   MOVEL     '1P'          COD
095500000829     C     KTAB2         SETLL     TABEL
095600000829     C     KTAB2         READE     TABEL                                  30
095700000829    1C     *IN30         DOWEQ     '0'
095800000829     C* CONTROLLO VALIDITA' TARIFFA PARTICOLARE PER LA GESTIONE TARIFFE
095900000829    2C     TBLFLG        IFEQ      *BLANKS
096000000829     C                   MOVEL     TBLUNI        DS1P
096100020204      * FLAG DI COMODO PER TABELLA 'SP'
096200020204     C                   CLEAR                   FLAGSP
096300080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
096400080609      * per le filiali non abilitate, accetto cartello
096500051012     c                   If        tblkey = 'c       '  and
096600080609     c                             kscfil <> 888
096700080609     c                   if        %lookup(kscfil:sklav) = *zeros
096800080609     c                             and %lookup(999:sklav) = *zeros
096900051012     c                   goto      doptab
097000080609     c                   endif
097100051012     c                   endif
097200130125       //?Se non posso manutenzionare le tariffe non carico le tariffe
097300130125       //?particolari NON sbloccate
097400130125     c                   IF        wBlocca and §1Pmodtp = *blanks
097500130125     c                   goto      doptab
097600130125     c                   ENDIF
097700051012      *
097800020204      * Carico solo le tariffe particolari che hanno flag = 'O'
097900020201      * Per tariffa con tipo servizio poste controllo flag §1pfpt
098000020201     C     PARTSP        IFEQ      'P'
098100020201     C     §1PFPT        ANDNE     'O'
098200020201     C                   GOTO      DOPTAB
098300020201     C                   ENDIF
098400020201      * Per tariffa con Network DPD controllo flag §1pfdp
098500020201     C     FLGNET        IFEQ      'D'
098600020201     C     §1PFDP        ANDNE     'O'
098700020201     C                   GOTO      DOPTAB
098800020201     C                   ENDIF
098900020201      * Per tariffa con Network FedEx controllo flag §1pffe
099000020201     C     FLGNET        IFEQ      'F'
099100020201     C     §1PFFE        ANDNE     'O'
099200020201     C                   GOTO      DOPTAB
099300020201     C                   ENDIF
099400020201      * Per tariffa Italia controllo flag §1ptco
099500020201     C     PARFIE        IFEQ      'I'
099600020205     C     PARTSP        ANDNE     'P'
099700020201     C     §1PTCO        ANDNE     'O'
099800020201     C                   GOTO      DOPTAB
099900020201     C                   ENDIF
100000020201      * Per tariffa Estero controllo flag §1pfie
100100020201     C     PARFIE        IFEQ      'E'
100200020205     C     FLGNET        ANDEQ     ' '
100300020201     C     §1PFIE        ANDNE     'O'
100400020201     C                   GOTO      DOPTAB
100500020201     C                   ENDIF
100600020201      *
100700020201      * Utilizzabile in tariffa
100800000829     C**
100900000829    3C     §1PUTA        IFEQ      'S'
101000000829     C*
101100000829     C*
101200000918     C                   MOVEL     TBLKEY        V09FTC
101300061011     C  n12KTPT9         CHAIN     TITPT000                           31
101400061011     c   12ktpt9         chain     tiopt000                           31
101500000829    4C     *IN31         IFEQ      '0'
101600000829     C* CONTROLLO VALIDITA' TARIFFA
101700000829     C     TPTATB        ANDEQ     ' '
101800000829     C* TARIFFA PARTICOLARE GIA' ESISTENTE
101900000829     C                   GOTO      DOPTAB
102000000829     C*
102100000829    4C                   ENDIF
102200000829     C* TARIFFA ANNULLATA   OPPURE
102300000829     C* TARIFFA PARTICOLARE NON ESISTENTE
102400000829     C*                     SCRIVO IL RECORD NEL SUBFILE
102500000829     C                   MOVEL     §1PDES        V09DES
102600020201      *
102700020201      * Per tariffa FedEx controllo se devo recupare la descrizione
102800020201      * dalla tabella SP
102900020201     C     FLGNET        IFEQ      'F'
103000020201     C     §1PDFE        ANDEQ     'S'
103100020204     C                   MOVEL     '1'           FLAGSP            1
103200020201     C                   CLEAR                   DSSP
103300020201     C                   MOVEL     'SP'          COD
103400020201     C                   MOVEL     *BLANKS       KEY
103500020201     C                   MOVEL     V09FTC        KEY
103600020204     C     KTAB          CHAIN     TABEL                              34
103700020201     C     *IN34         IFEQ      *OFF
103800020201     C                   MOVEL     TBLUNI        DSSP
103900020201     C                   ENDIF
104000020201     C     §SPDFE        IFNE      *BLANKS
104100020201     C                   MOVEL     §SPDFE        V09DES
104200020201     C                   ENDIF
104300020201     C                   ENDIF
104400020201      *
104500000829     C* CAMPO SELEZIONE
104600000829     C                   MOVEL     ' '           V09SCE
104700000829     C                   ADD       1             NR9
104800000829     C                   WRITE     TA67S09
104900000829    3C                   END
105000000829    2C                   END
105100000829     C*
105200000829     C     DOPTAB        TAG
105300020204     C     FLAGSP        IFNE      *BLANKS
105400020201     C                   MOVEL     '1P'          COD
105500020204     C     KTAB          SETGT     TABEL
105600020204     C                   ENDIF
105700000829     C     KTAB2         READE     TABEL                                  30
105800000829    1C                   END
105900000829     C*
106000000829     C* GESTIONE DEL SUBFILE DELLA COPIA TARIFFE
106100000915     C                   SETON                                        81
106200000918     C     NR9           COMP      0                                      80
106300000829     C                   Z-ADD     1             NR9
106400000829     C                   WRITE     TA67T01
106500000829     C                   WRITE     TA67Z09
106600000829     C                   EXFMT     TA67C09
106700000829     C*
106800000829    1C     *INKL         IFNE      *ON
106900070521     c     *in80         andeq     *off
107000000829     C* LEGGO IL SUBFILE
107100000901    2C                   DO        *HIVAL
107200000829     C                   READC     TA67S09                                30
107300000915    3C     *IN30         IFEQ      *ON
107400000829     C                   LEAVE
107500000901    3C                   ENDIF
107600000901    3C     V09SCE        IFEQ      '1'
107700061011     C* CARICO LA DS PER PGM DI COPIA TARIFFA
107800061011     c                   clear                   tnta25ds
107900061011     c  n12              eval      ta25tipo = 'T'
108000061011     c   12              eval      ta25tipo = 'O'
108100061011     c                   eval      ta25ksco = vidcli
108200061011     c                   move      vidctr        ta25ctro
108300061011     c                   move      vidprg        ta25prgo
108400061011     c  n12              eval      ta25tipn = 'T'
108500061011     c   12              eval      ta25tipn = 'O'
108600061011     c                   eval      ta25kscn = vidcli
108700061011     c                   move      vidctr        ta25ctrn
108800061011     c                   move      vidprg        ta25prgn
108900061011     c                   eval      ta25tam = 'N'
109000061011     c                   eval      ta25fie = parfie
109100061011     c                   eval      ta25tam = 'N'
109200061011     c                   eval      ta25tad = 'N'
109300061011     c                   eval      ta25tpt = 'N'
109400061011     c                   eval      ta25tpd = 'N'
109500061011     c                   eval      ta25tgc = 'N'
109600061011     c                   eval      ta25cat = 'N'
109700061011     c                   eval      ta25inglo = 'S'
109800061011
109900061011     c                   eval      ta25ftcr = v09ftc
110000061011
110100061011     c                   call      'TNTA25R'
110200061011     c                   parm                    kpjba
110300061011     c                   parm                    tnta25ds
110400061011     c                   parm                    tnta25tad
110500061011     c                   parm                    tnta25tgc
110600061011     c                   parm                    tnta25tpd
110700061011     c                   parm                    tnta25tpt
110800000829     C*
110900000901    3C                   ENDIF
111000000901    2C                   ENDDO
111100000829     C*
111200000901    1C                   ENDIF
111300000829     C*
111400000829     C                   ENDSR
111500920529     C*
111600920529     C*--- SR SECONDA VIDEATA ----------------------------------------*
111700920529     C     VIDEO2        BEGSR
111800920619     C                   SETOFF                                       010207
111900920619     C                   SETOFF                                       090304
112000941116     C                   SETOFF                                       062425
112100060303
112200060303     c                   eval      *in78 = *off
112300060303
112400920529     C*
112500941116     C* 24 ON - TIPO CONSEGNA = "C - ASSICURAZIONE X CONTO"
112600990217     C     §1PASC        IFEQ      'S'
112700990217     C                   SETON                                            24
112800990217     C                   ENDIF
112900941116     C* 25 ON - TIPO CONSEGNA = "R - RCV"
113000990217     C     §1PRCV        IFEQ      'S'
113100990217     C                   SETON                                            25
113200990217     C                   ENDIF
113300941110     C*
113400920609     C* TARIFFA PARTICOLARE NON ESISTENTE
113500920708     C     VIDFLG        IFEQ      ' '
113600920529     C                   SETON                                        01
113700920529     C                   ELSE
113800920609     C* TARIFFA PARTICOLARE GIA' ESISTENTE
113900920529     C                   SETON                                        02
114000920529     C                   END
114100920529     C*
114200921006     C* PULIZIA CAMPI DEL FORMATO E DEL SFLCONTROL
114300921006     C                   MOVEL     ' '           VI2TPG
114400920529     C                   EXSR      PULCTL
114500921126     C*
114600921005     C* CARICO TIPO TARIFFA
114700061011     C   02
114800061011     Cann12KTPT          CHAIN     TITPT000                           33
114900061011     c   02
115000061011     can 12ktpt          chain     tiopt000                           33
115100921005     C   02
115200921005     CANN33              MOVEL     TPTTPG        VI2TPG
115300921005     C   02
115400921005     CANN33              MOVEL     TPTTPG        SAVTPG
115500921118     C   01              MOVEL     ' '           SAVTPG
115600060303
115700060303      * se tariffa particolare "d"
115800060303      * i dati vanno protetti
115900060303     c                   if        vidftc = 'd '
116000060303     c                   eval      *in78 = *on
116100060303      * se non è presente carico i dati di dft
116200060303     c                   if        *in01
116300060303     c                   eval      vi2tpg = '3'
116400060303     c                   endif
116500060303     c                   endif
116600921005     C*
116700921005     C     FOR05         TAG
116800921005     C                   SETOFF                                       22
116900960520     C*
117000940913     C                   WRITE     TA67T01
117100940913     C                   WRITE     TA67Z08
117200921005     C     FOR06         TAG
117300941110     C                   EXFMT     TA67D06
117400921005     C*
117500921005     C** CMD12 - FINE LAVORO
117600921005     C   KL              GOTO      FINE2
117700921005     C*
117800921005     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
117900921005     C   KQ              EXSR      ANNTAR
118000921005     C   KQ              GOTO      FINE2
118100921005     C*
118200921005     C* CONTROLLI FORMATO
118300921005     C                   EXSR      CONTR2
118400921005     C   98              GOTO      FOR05
118500921005     C*
118600921005     C   99              GOTO      FOR06
118700921005     C*
118800921005     C     INVID2        TAG
118900921007     C                   Z-ADD     0             NR2
119000921007     C                   Z-ADD     0             SAVNR2
119100921005     C                   SETON                                        22
119200921005     C* CARICO DATI DEL SFLCONTROL
119300921215     C   02
119400921215     CANN29              MOVEL     TPTDS         COMTPT
119500921006     C   02
119600921006     CANN29              EXSR      CARCTL
119700060303
119800060303      * se tariffa particolare "d" non presente carico i dati di dft
119900060303     c                   if        *in01 and vidftc = 'd '
120000060303      * valore merce 6,2
120100060303     c                   z-add     6,2           vi2vlm
120200060303     c                   z-add     vi2vlm        savvlm
120300060303      * tipo valore '3' già impostato in precedenza se immissione
120400060303     c                   endif
120500060802      * se tariffa particolare "f" non presente carico i dati di dft
120600060802     c                   if        *in01 and vidftc = 'f '
120700060802     c                   seton                                        86
120800061207      *? ATTENZIONE CHIODO!!!  ?
120900061207      * se data del giorno è inferiore a 31/12/2006
121000061207      * chiodo fisso 31/12/2006
121100061207      * altrimenti metto la data del giorno
121200061207     c                   if        dateu8 < 20061231
121300061207     c                   eval      vi2dpb = 31122006
121400061207     c                   else
121500060802     c                   z-add     udate8        vi2dpb
121600061207     c                   endif
121700060802     c                   endif
121800060303      *
121900920611     C* PULIZIA SUBFILE
122000920611     C                   EXSR      PULSF2
122100941110     C*
122200970520     C* 06 ON - APPLICAZIONE CON INOLTRO  PER ISOLE/LOC.DISAGIATE/C.STO
122300970520     C     §1PAIN        COMP      'S'                                    06
122400941110     C*
122500920529     C* CARICO SUBFILE
122600921214     C   02              EXSR      CARSFL
122700920609     C* SCRIVO RECORD DI NUOVA IMMISSIONE
122800921005     C   01              EXSR      SCRIVI
122900920617     C*
123000920617     C                   Z-ADD     1             REC2
123100920611     C     FOR01         TAG
123200940913     C                   WRITE     TA67T01
123300940913     C                   WRITE     TA67Z05
123400941110     C                   WRITE     TA67D06
123500920529     C     FORCT         TAG
123600941110     C                   EXFMT     TA67C07
123700950119     C*
123800950119     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN13)
123900950119     C                   CLEAR                   VI2MSG
124000950119     C                   SETOFF                                       1374
124100111130     c                   setoff                                       62
124200920609     C*
124300111122     C** CMD12 - FINE LAVORO
124400111122     c                   If        *Inkl  and *in62
124500111122     C                   SETON                                        99
124600111122     c                   goto      forct
124700111122     C                   ENDIF
124800111122     c
124900920611     C** CMD12 - FINE LAVORO
125000050704     c                   If        *Inkl
125100111122      /free
125200111122       // Non posso fare F12 se ho annullato tutte le righe di dettaglio
125300111130        IF not *in01;
125400111122         *in62 = *on;
125500111123         nr2 = 1;
125600111123         chain nr2 TA67S07;
125700111123         DOW %found and *in62;
125800111130           IF  VI2cts <> *blanks;
125900111123             *in62 = *off;
126000111123           ENDIF;
126100111123           update TA67S07;
126200111122           nr2 += 1;
126300111122           chain nr2 TA67S07;
126400111122         ENDDO;
126500111123      /end-free
126600111123     c                   IF        *in62
126700111123     c                   eval      *in99 = *in62
126800111123     c                   goto      forct
126900111123     c                   ENDIF
127000111130      /free
127100111130          *in62 = *on;
127200111130          If not *in12 ;
127300111130          setll (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
127400111130          reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
127500111130          DOW  not %eof(TITPD01L);
127600111130            IF  TPDatb = *blanks;
127700111130              *IN62 = *off;
127800111130            ENDIF;
127900111130            reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
128000111130          ENDDO;
128100111130          Endif ;
128200111130          If *in12 ;
128300111130            setll (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
128400111130            reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
128500111130            DOW  not %eof(TIOPD01L);
128600111130              IF  TPDatb = *blanks;
128700111130               *IN62 = *off;
128800111130             ENDIF;
128900111130             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
129000111130           ENDDO;
129100111130           Endif ;
129200111130      /end-free
129300111130     c                   IF        *in62
129400111130     c                   eval      *in99 = *in62
129500111130     c                   goto      forct
129600111130     c                   ENDIF
129700111130     c                   ENDIF
129800111122     c
129900050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
130000050704      * per codice di tassazione
130100050704     c                   Clear                   savcts
130200050704     c                   If        vi2tpg = '2'
130300061011     c  n12ktpt          Setll     titpd01l
130400061011     c   12ktpt          Setll     tiopd01l
130500050704     c                   Do        *Hival
130600061011     c  n12ktpt          Reade(n)  titpd01l
130700061011     c   12ktpt          Reade(n)  tiopd01l
130800061011     c                   If        %eof
130900050704     c                   Leave
131000050704     c                   EndIf
131100050704     c                   If        tpdatb <> *Blanks
131200050704     c                   Iter
131300050704     c                   EndIf
131400050704     c                   If        tpdcts = savcts
131500050704     c                   Eval      *in69 = *on
131600050704     c                   Eval      *in13 = *on
131700050704     c                   Eval      vi2msg = msg(6)
131800050704     c                   Else
131900050704     c                   Eval      savcts = tpdcts
132000050704     c                   EndIf
132100050704     c                   EndDo
132200050704     c                   If        *In13
132300050704     c                   Goto      invid2
132400050704     c                   EndIf
132500050704     c                   EndIf
132600050704     C                   GOTO      FOR05
132700050704     c                   EndIf
132800920611     C*
132900920611     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
133000920611     C   KQ              EXSR      ANNTAR
133100920611     C   KQ              GOTO      FINE2
133200920611     C*
133300920611     C** CMD9  - AGGIUNTA RIGHE
133400920611     C   KI
133500920529     C* SCORRIMENTO
133600920611     COR 08              DO
133700920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
133800920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
133900920619     C                   SETON                                        0336
134000920611     C                   EXSR      SCRIVI
134100920529     C                   GOTO      FORCT
134200920529     C                   END
134300920529     C*
134400920611     C* CONTROLLO DATI SUBFILE CONTROL
134500920611     C                   EXSR      CONCTL
134600941111     C   98              GOTO      FOR01
134700941111     C*
134800920619     C   99              GOTO      FORCT
134900920529     C*
135000921007     C* CONTROLLO DATI SUBFILE
135100921007     C                   EXSR      CONSFL
135200921007     C   99              GOTO      FORCT
135300921007     C*
135400920623     C** ENTER - REGISTRA
135500920624     C* REGISTRO CAMPI DEL SUBFILE
135600920709     C                   EXSR      REGIS
135700920624     C* REGISTRO CAMPI DEL SUBFILE CONTROL
135800920709     C                   EXSR      REGIS2
135900920623     C*
136000921007     C* SPENGO L' *IN29  PERCHE' IN QUESTO MODO RIESCO A POSIZIONARE
136100921007     C*   IL CURSORE SULLA PRIMA RIGA DI DETTAGLIO  - (ALTRIMENTI
136200921007     C*   RESTAVA POSIZIONATO SUL PRIMO CAMPO DEL SFLCONTROL) -
136300921007     C                   SETOFF                                       29
136400111123
136500111123      /free
136600111123       // Se ho dato F6 o F14 verifico di avere almeno 1 rcd di dettaglio
136700111123         IF  *inkf or *inkn;
136800111123           *in62 = *on;
136900111130           If not *in12 ;
137000111123           setll (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
137100111123           reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
137200111123           DOW  not %eof(TITPD01L);
137300111123             IF  TPDatb = *blanks;
137400111123               *IN62 = *off;
137500111123             ENDIF;
137600111123             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
137700111123           ENDDO;
137800111130           Endif ;
137900111130           If *in12 ;
138000111130           setll (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
138100111130           reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
138200111130           DOW  not %eof(TIOPD01L);
138300111130             IF  TPDatb = *blanks;
138400111130               *IN62 = *off;
138500111130             ENDIF;
138600111130             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
138700111130           ENDDO;
138800111130           Endif ;
138900111123         ENDIF;
139000111123      /end-free
139100111123     c                   IF        *in62
139200111123     c                   eval      *in99 = *in62
139300111123     c                   goto      invid2
139400111123     c                   ENDIF
139500050704
139600050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
139700050704      * per codice di tassazione
139800050704     c                   Clear                   savcts
139900050704     c                   If        vi2tpg = '2'
140000061011     c  n12ktpt          Setll     titpd01l
140100061011     c   12ktpt          Setll     tiopd01l
140200050704     c                   Do        *Hival
140300061011     c  n12ktpt          Reade(n)  titpd01l
140400061011     c   12ktpt          Reade(n)  tiopd01l
140500061011     c                   If        %eof
140600050704     c                   Leave
140700050704     c                   EndIf
140800050704     c                   If        tpdatb <> *Blanks
140900050704     c                   Iter
141000050704     c                   EndIf
141100050704     c                   If        tpdcts = savcts
141200050704     c                   Eval      *in69 = *on
141300050704     c                   Eval      *in13 = *on
141400050704     c                   Eval      vi2msg = msg(6)
141500050704     c                   Else
141600050704     c                   Eval      savcts = tpdcts
141700050704     c                   EndIf
141800050704     c                   EndDo
141900050704     c                   If        *In13
142000050704     c                   Goto      invid2
142100050704     c                   EndIf
142200050704     c                   EndIf
142300050704
142400921007     C*
142500920623     C** CMD6  - REGISTRA ED ESCE
142600921202     C  NKF
142700921202     CANNKN              GOTO      INVID2
142800920529     C*
142900920708     C     FINE2         ENDSR
143000920609     C*
143100920609     C*--- PULIZIA SUBFILE CONTROL -----------------------------------*
143200920609     C     PULCTL        BEGSR
143300930223     C                   MOVEL     ' '           UNO               1
143400920609     C                   Z-ADD     0             VI2ARL
143500940712     C                   Z-ADD     0             VI2ARF
143600920609     C                   Z-ADD     0             VI2ARO
143700930217     C                   Z-ADD     0             VI2RPV
143800980422     C                   MOVE      ' '           VI2APL
143900980428     C                   MOVE      ' '           VI2MAS
144000060802     C                   Z-ADD     0             VI2DPB
144100060802     C                   Z-ADD     0             W_VI2DPB
144200941111     C                   CLEAR                   TPG1
144300941111     C                   CLEAR                   DPG1
144400941111     C                   CLEAR                   TPG2
144500941111     C                   CLEAR                   DPG2
144600941111     C                   CLEAR                   TPG3
144700941111     C                   CLEAR                   DPG3
144800941111     C                   CLEAR                   TPG4
144900941111     C                   CLEAR                   DPG4
145000941111     C                   CLEAR                   TPG5
145100941111     C                   CLEAR                   DPG5
145200941111     C                   CLEAR                   TPG6
145300941111     C                   CLEAR                   DPG6
145400980508     C                   CLEAR                   TPG7
145500980508     C                   CLEAR                   DPG7
145600980508     C                   CLEAR                   TPG8
145700980508     C                   CLEAR                   DPG8
145800941111     C*
145900941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
146000941111    1C     *IN11         IFEQ      '1'
146100930217     C   24              Z-ADD     0             VI2VLM
146200930217     C*
146300930217     C* IN IMMISSIONE PER IL TIPO CONSEGNA "R", NEL CAMPO LIMITE
146400941111     C*    RISARCIBILE PROPONGO 12000 PRENDENDOLO DALLA TABELLA "1G"
146500990907     C   25              Z-ADD     §GELRP        VI2VLM
146600930217     C*
146700930223     C* IN IMMISSIONE PROPONGO IL CAMPO TIPO VALORE = 3-PESO KG.
146800930223     C                   MOVEL     '3'           VI2FVM
146900941111     C* DECODIFICA TIPO VALORE MERCE
147000930223     C                   MOVEL     *BLANKS       DESFVM
147100980316     C* DECODIFICA DALLA TABELLA TR
147200980316     C                   MOVEL     'TR'          COD
147300930223     C                   MOVEL     *BLANKS       KEY
147400930223     C                   MOVEL     VI2FVM        KEY
147500930223     C     KTAB          CHAIN     TABEL                              34
147600941111    2C     *IN34         IFEQ      '0'
147700980316     C                   MOVEL     TBLUNI        DSTR
147800980316     C* DECODIFICA DALLA TABELLA TR
147900980316     C                   MOVEL     §TRDES        DESFVM
148000941111    2C                   END
148100941111    1C                   END
148200941110     C*
148300980317     C* CARICO I TIPI TARIFFA
148400941110     C     TTR(1)        IFNE      ' '
148500941110     C                   MOVEL     TTR(1)        TPG1
148600941110     C                   MOVEL     DTR(1)        DPG1
148700941110     C                   END
148800941110     C     TTR(2)        IFNE      ' '
148900941110     C                   MOVEL     TTR(2)        TPG2
149000941110     C                   MOVEL     DTR(2)        DPG2
149100941110     C                   END
149200941110     C     TTR(3)        IFNE      ' '
149300941110     C                   MOVEL     TTR(3)        TPG3
149400941110     C                   MOVEL     DTR(3)        DPG3
149500941110     C                   END
149600941110     C     TTR(4)        IFNE      ' '
149700941110     C                   MOVEL     TTR(4)        TPG4
149800941110     C                   MOVEL     DTR(4)        DPG4
149900941110     C                   END
150000941110     C     TTR(5)        IFNE      ' '
150100941110     C                   MOVEL     TTR(5)        TPG5
150200941110     C                   MOVEL     DTR(5)        DPG5
150300941110     C                   END
150400941110     C     TTR(6)        IFNE      ' '
150500941110     C                   MOVEL     TTR(6)        TPG6
150600941110     C                   MOVEL     DTR(6)        DPG6
150700941110     C                   END
150800980428     C     TTR(7)        IFNE      ' '
150900980428     C                   MOVEL     TTR(7)        TPG7
151000980428     C                   MOVEL     DTR(7)        DPG7
151100980428     C                   END
151200980428     C     TTR(8)        IFNE      ' '
151300980428     C                   MOVEL     TTR(8)        TPG8
151400980428     C                   MOVEL     DTR(8)        DPG8
151500980428     C                   END
151600941110     C*
151700941111     C                   MOVEL     *BLANKS       COMTPT           38
151800920828     C                   ENDSR
151900921005     C*
152000941116     C*--- CONTROLLO DATI FORMATO ------------------------------------*
152100921005     C     CONTR2        BEGSR
152200921006     C                   SETOFF                                       262729
152300941110     C                   SETOFF                                       989921
152400060802     C                   SETOFF                                       14
152500950119     C                   CLEAR                   WSCD
152600921005     C*
152700921005     C* TIPO TARIFFA OBBLIGATORIO
152800941116    1C     VI2TPG        IFEQ      '?'
152900941110     C                   MOVEL     'TR'          COD
153000941110     C*
153100921005     C                   MOVE      ' '           VI2TPG
153200001020     C                   MOVE      *BLANKS       KEY
153300921005     C                   MOVEL     *BLANKS       DES              25
153400921005     C                   CALL      'X§TABER'
153500921005     C                   PARM                    CODUT
153600921005     C                   PARM                    COD
153700921005     C                   PARM                    KEY
153800921005     C                   PARM                    DES
153900921005     C                   MOVEL     KEY           VI2TPG
154000941110     C                   SETON                                        98
154100921005     C                   GOTO      END2
154200941116    1C                   END
154300921005     C*
154400941116    1C     VI2TPG        IFEQ      ' '
154500921005     C                   SETON                                        4099
154600941116    1C                   END
154700941116     C*
154800921005     C* CONTROLLO SE ESISTE TIPO TARIFFA
154900941020     C                   Z-ADD     1             X
155000941020     C     VI2TPG        LOOKUP    TTR(X)                                 20
155100941110     C* CONTROLLO SE TARIFFA IN % O IN £
155200941110     C   20TTP(X)        COMP      'S'                                    14
155300950119     C* WSCD = "S" ---> SCAGLIONE CON DECIMALI
155400950119     C   20              MOVEL     TSP(X)        WSCD              1
155500980429     C* ARROTONDAMENTI
155600980429     C   20              MOVEL     ARR(X)        WARR              1
155700980429     C* RAPPORTO PESO VOLUME
155800980429     C   20              MOVEL     RPV(X)        WRPV              1
155900941110     C*
156000941110     C*
156100921005     C  N20              SETON                                        4199
156200921005     C   99              GOTO      END2
156300941020     C*
156400950119     C* I TIPI TARIFFA "4" E "5" SI POSSONO ACCETTARE SOLO SE LA TARIFFA
156500950119     C*   CLIENTE HA COME COD.TARIFFA RISPETTIVAMENTE "4xx" E "5xx"
156600021209      * se in tabella '1P' il flag §1PFQT è a Blank
156700021209     c                   If        Wfqt = *Blanks
156800950119     C                   MOVEL     VIDCTR        WCTR              1
156900950119    1C     VI2TPG        IFEQ      '4'
157000950119     C     WCTR          ANDNE     '4'
157100950119     C                   SETON                                        7099
157200950119     C                   GOTO      END2
157300950119    1C                   ENDIF
157400950119    1C     VI2TPG        IFEQ      '5'
157500950119     C     WCTR          ANDNE     '5'
157600950119     C                   SETON                                        7399
157700950119     C                   GOTO      END2
157800950119    1C                   ENDIF
157900021209     c                   EndIf
158000950119     C*
158100950112     C* IN VARIAZIONE: SE TARIFFA A % NON POSSO VARIARE IL TIPO TARIFFA
158200950119    1C   02SAVTPG        IFNE      VI2TPG
158300950119     C                   CLEAR                   WCOM
158400950119     C                   Z-ADD     1             X
158500950119     C*
158600950119     C     SAVTPG        LOOKUP    TTR(X)                                 20
158700950119     C* CONTROLLO SE TARIFFA IN % O IN £
158800950119     C   20TTP(X)        IFEQ      'S'
158900950119     C                   MOVEL     'P'           WCOM              1
159000950119     C                   ENDIF
159100950119     C*
159200950119    2C     *IN14         IFEQ      *ON
159300950119     C     WCOM          ANDEQ     ' '
159400950119     C     *IN14         OREQ      *OFF
159500950119     C     WCOM          ANDEQ     'P'
159600941116     C                   SETON                                        5199
159700941116     C                   GOTO      END2
159800950112    2C                   ENDIF
159900950112    1C                   ENDIF
160000941110     C*
160100921016     C* OGNI VOLTA CHE CAMBIO IL TIPO TARIFFA DEVO AZZERARE TUTTI
160200921016     C*   I CAMPI DEL SFLCONTROL
160300941116    1C     VI2TPG        IFNE      SAVTPG
160400921006     C                   SETON                                        29
160500921005     C                   EXSR      PULCTL
160600941116    1C                   END
160700921005     C*
160800921005     C* CONDIZIONO L'EMISSIONE DEGLI ARROTOND. E DEL RAPPORTO PESO/VOL.
160900980429     C     WARR          IFEQ      'S'
161000980429     C                   SETON                                        26
161100980429     C                   END
161200980429     C*
161300980429     C     WRPV          IFEQ      'S'
161400980429     C                   SETON                                        27
161500980429     C                   END
161600941110     C*
161700941110     C* 21 ON - SE TIPO TARIFFA = 0  PROTEGGO IL CAMPO TIPO VALORE
161800981027     C   25              SETON                                        21
161900921005     C*
162000921005     C     END2          ENDSR
162100920529     C*
162200920611     C*--- PULIZIA SUBFILE -------------------------------------------*
162300920611     C     PULSF2        BEGSR
162400920611     C                   SETON                                        37
162500941111     C                   WRITE     TA67C07
162600920611     C                   SETOFF                                       37
162700920611     C                   ENDSR
162800920611     C*
162900920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
163000921214     C     CARSFL        BEGSR
163100920609     C                   SETOFF                                       323399
163200050704     c                   clear                   savcts
163300920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
163400920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
163500920619     C                   SETON                                        050436
163600920616     C*
163700920611     C                   MOVEL     'CT'          COD
163800920611     C                   MOVEL     *BLANKS       KEY
163900920529     C* RICERCA
164000061011     C  n12KTPT          SETLL     TITPD000
164100061011     c   12ktpt          setll     tiopd000
164200920608     C                   Z-ADD     0             NR2               4 0
164300061011     C  n12KTPT          READE     TITPD000                               32
164400061011     c   12ktpt          reade     tiopd000                               32
164500920529     C* 32 ON - FINE LETTURA
164600920611     C     *IN32         DOWEQ     '0'
164700020207      *
164800050704     C                   SETOFF                                       767769
164900020207      *
165000920611     C* CONTROLLO VALIDITA' TARIFFA
165100920611     C     TPDATB        IFEQ      ' '
165200920529     C* CODICE TASSAZIONE
165300920529     C                   MOVEL     TPDCTS        VI2CTS
165400920608     C* DECODIFICA CODICE TASSAZIONE
165500920609     C                   MOVEL     VI2CTS        KEY
165600920608     C     KTAB          CHAIN     TABEL                              33
165700920612     C  N33              DO
165800920612     C                   MOVEL     TBLUNI        DSCT
165900920612     C                   MOVEL     §CTDES        VI2DCT
166000920612     C                   END
166100920608     C   33              MOVEL     *BLANKS       VI2DCT
166200020207      *
166300020207      * Se è tariffa FedEx devo copiare solo i codici tassazioni
166400020207      * previsti per FedEx
166500020207     C     FLGNET        IFEQ      'F'
166600020207     C     §CTUTF        ANDNE     'S'
166700020207     C                   SETON                                        76
166800020207     C     VI2MSG        IFEQ      *BLANKS
166900020207     C                   SETON                                        13
167000020207     C                   MOVEA     MSG(2)        VI2MSG
167100020207     C                   ENDIF
167200020207     C                   ENDIF
167300020207      * Se NON è tariffa FedEx devo copiare solo i codici tassazioni
167400020207      * NON previsti per FedEx
167500020207     C     FLGNET        IFNE      'F'
167600020207     C     §CTUTF        ANDEQ     'S'
167700020207     C                   SETON                                        77
167800020207     C     VI2MSG        IFEQ      *BLANKS
167900020207     C                   SETON                                        13
168000020207     C                   MOVEA     MSG(3)        VI2MSG
168100020207     C                   ENDIF
168200020207     C                   ENDIF
168300050704      *
168400050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
168500050704      * per codice di tassazione
168600050704     c                   If        vi2tpg = '2'
168700050704     c                   If        tpdcts = savcts
168800050704     c                   Eval      *In69 = *On
168900050704     c                   If        vi2msg = *Blanks
169000050704     c                   Eval      *In13 = *On
169100050704     c                   Movea     msg(6)        vi2msg
169200050704     c                   EndIf
169300050704     c                   Else
169400050704     c                   Eval      savcts = tpdcts
169500050704     c                   EndIf
169600050704     c                   EndIf
169700020207      *
169800970318     C                   MOVEL     TPDORP        VI2ORP
169900920615     C* SCAGLIONE
170000940712     C                   MOVEL     TPDSGL        VI2SGL
170100920608     C* IMPORTO TARIFFA
170200941110     C                   Z-ADD     TPDITR        VI2ITR
170300941110     C                   Z-ADD     TPDITR        SAVITR
170400960520     C                   Z-ADD     TPDITR        SA2ITR
170500920608     C* MINIMO TARIFFA
170600990614     C                   Z-ADD     TPDMIN        VI2MIN
170700990614     C                   Z-ADD     TPDMIN        SAVMIN
170800940713     C* MASSIMO TARIFFA
170900990614     C                   Z-ADD     TPDMAX        VI2MAX
171000990614     C                   Z-ADD     TPDMAX        SAVMAX
171100920611     C* 06 ON - TIPO CONSEGNA = I  CARICO FLAG TARIFFA PROVINCIA
171200920611     C   06              MOVEL     TPDAIN        VI2AIN
171300930219     C   06              MOVEL     TPDAIN        SAVAIN
171400930219     C  N06              MOVEL     ' '           SAVAIN
171500920610     C*
171600061011     C  n12              Z-ADD     TPDNRR        VI2NRR
171700061011     c   12              z-add     tponrr        vi2nrr
171800920608     C* FLAG ANNULLAMENTO
171900920610     C                   MOVEL     ' '           VI2ATB
172000920610     C*
172100920608     C                   ADD       1             NR2
172200920608     C*
172300941111     C                   WRITE     TA67S07
172400920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
172500920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
172600920709     C                   SETOFF                                       0436
172700920611     C                   END
172800920608     C*
172900061011     C  n12KTPT          READE     TITPD000                               32
173000061011     c   12ktpt          reade     tiopd000                               32
173100920611     C                   END
173200920608     C*
173300920609     C                   Z-ADD     NR2           SAVNR2            4 0
173400920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
173500920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
173600920619     C                   SETOFF                                       0436
173700920608     C*
173800920616     C     NR2           IFGT      0
173900920608     C     NR2           DIV       9             DIVIS             3 0
174000920608     C                   MVR                     RESTO             3 0
174100920611     C     RESTO         IFGT      0
174200920611     C                   EXSR      SCRIVI
174300920611     C                   END
174400921126     C*
174500920616     C                   ELSE
174600920618     C                   SETON                                        03
174700920616     C                   EXSR      SCRIVI
174800920616     C                   END
174900920611     C*
175000920608     C                   ENDSR
175100920608     C*
175200920609     C*--- CARICO DATI SUBFILE CONTROL -------------------------------*
175300920608     C     CARCTL        BEGSR
175400061011     C  n12KTPT          CHAIN     TITPT000                           33
175500061011     c   12ktpt          chain     tiopt000                           33
175600941111    1C     *IN33         IFEQ      '0'
175700920925     C*
175800941111     C* 26 ON  - VISUALIZZO ARROTONDAMENTI
175900941111    2C     *IN26         IFEQ      *ON
176000990614     C                   Z-ADD     TPTARL        VI2ARL
176100990614     C                   Z-ADD     TPTARF        VI2ARF
176200990614     C                   Z-ADD     TPTARO        VI2ARO
176300941111    2C                   END
176400941111     C* 27 ON  - VISUALIZZO RAPPORTO PESO/VOLUME
176500920925     C   27              Z-ADD     TPTRPV        VI2RPV
176600920924     C*
176700941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
176800941111    2C     *IN11         IFEQ      *ON
176900920924     C* VALORE MERCE/LIMITE RISARCIBILE
177000920924     C                   Z-ADD     TPTVLM        VI2VLM
177100921009     C                   Z-ADD     TPTVLM        SAVVLM
177200941111     C* TIPO VALORE MERCE
177300940712     C                   MOVEL     TPTFVM        VI2FVM
177400941111     C* DECODIFICA TIPO VALORE MERCE
177500921007     C                   MOVEL     *BLANKS       DESFVM
177600980316     C                   MOVEL     'TR'          COD
177700921007     C                   MOVEL     *BLANKS       KEY
177800921007     C                   MOVEL     VI2FVM        KEY
177900921007     C     KTAB          CHAIN     TABEL                              34
178000941111    3C     *IN34         IFEQ      '0'
178100980316     C                   MOVEL     TBLUNI        DSTR
178200980316     C                   MOVEL     §TRDES        DESFVM
178300941111    3C                   END
178400941111    2C                   END
178500941111     C*
178600941111     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
178700941111     C   28              MOVEL     TPTAPL        VI2APL
178800990217     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO
178900990903     C   43              MOVEL     TPTTMA        VI2MAS
179000060802      * 82 ON  - Visualizzo la data riferimento prezzo base del carburante
179100060802     c                   If        *in82
179200060802     c                   clear                   WLBdat
179300060802     c                   eval      G08inv = tptdpb
179400060802     c                   eval      G08err = '3'
179500060802     c                   call      'XSRDA8'
179600060802     c                   parm                    WLBdat
179700060802     c                   z-add     G08dat        vi2dpb
179800060802     c                   endif
179900941111    1C                   END
180000060802     C
180100920924     C*
180200920608     C                   ENDSR
180300920609     C*
180400920609     C*--- SR AGGIUNTA RIGHE -----------------------------------------*
180500920609     C     SCRIVI        BEGSR
180600920611     C                   SETOFF                                       05
180700020207     C                   SETOFF                                       7677
180800920611     C                   Z-ADD     0             B                 3 0
180900920611     C     RESTO         IFNE      0
181000920611     C                   Z-ADD     RESTO         B
181100920611     C                   Z-ADD     0             RESTO
181200920611     C                   END
181300920609     C* CAMPI DEL SUBFILE
181400920609     C                   Z-ADD     SAVNR2        NR2
181500060306
181600060306      * se tariffa particolare "d" imposto i dati di dft
181700140127      * + il prezzo recuperato dal file TIACB
181800060306      * e scrivo una riga
181900060306     c                   if        vidftc = 'd ' and *in01
182000060307     c                   eval      *in79 = *on
182100060306     c                   eval      vi2cts = 'IT'
182200060306     c                   eval      vi2sgl = 9999999
182300140127     c******             select
182400060306      * clienti Top o Direzionali
182500140127     c******             when      clpclv = 'T' or clpclv = 'D'
182600140116     c*****              eval      vi2itr = 0,003
182700111130     c****               eval      vi2itr = 0,002
182800060306      * clienti A
182900140127     c******             when      clpclv = 'A'
183000140116     c*****              eval      vi2itr = 0,004
183100111130     c****               eval      vi2itr = 0,003
183200060306      * clienti C o B
183300140127     c******             when      clpclv = 'C' or clpclv = 'B'
183400140116     c*****              eval      vi2itr = 0,007
183500111130     c****               eval      vi2itr = 0,005
183600060314      * non trovo la categoria cliente imposto il massimo
183700140127     c******             other
183800140116     c*****              eval      vi2itr = 0,007
183900111130     c****               eval      vi2itr = 0,005
184000140127     c******             endsl
184100140127
184200140127       //?Recupero la tariffa AC BASE dal file TIACB
184300140127     c     kACB          setll     TIACB01L
184400140127     c     CLPclv        reade     TIACB01L
184500140127     c                   if        %found(TIACB01L)
184600140127     c                   eval      VI2itr = ACBitr
184700140127     c                   ENDIF
184800140127
184900060705      * controllo se il cliente è presente sulla tabella della forzatura
185000060705      * AC BASE e recupero la tariffa dalla tabella
185100060705     c                   clear                   dsbs02
185200060705     c                   clear                   dfab
185300060705     c                   eval      t02mod = 'C'
185400060705     c                   eval      t02sif = knsif
185500060705     c                   eval      t02cod = 'FAB'
185600060705     c  n12              movel     vidcli        t02ke1
185700060705     c   12              movel     visksc        t02ke1
185800060705     c                   call      'TIBS02R'
185900060705     c                   parm                    kpjba
186000060705     c                   parm                    dsbs02
186100060705     c                   if        t02err = *blanks
186200060705     c                   eval      dfab = t02uni
186300060705     c                   endif
186400060705     c                   if        d§fabitr > 0
186500060705     c                   eval      vi2itr = d§fabitr
186600060705     c                   endif
186700060705
186800060306     c                   z-add     0,001         vi2min
186900060306     c                   z-add     0             vi2max
187000060306     c                   z-add     vi2itr        savitr
187100060306     c                   z-add     vi2itr        sa2itr
187200060306     c                   z-add     vi2min        savmin
187300060306     c                   z-add     0             savmax
187400060306     c                   z-add     0             vi2nrr
187500060306     c                   add       1             nr2
187600060306     c                   add       1             b
187700060306     c                   write     ta67s07
187800060306     c                   setoff                                       0336
187900060306     c                   endif
188000920609     C*
188100920625     C     B             DOWLT     9
188200920609     C                   MOVEL     *BLANKS       VI2CTS
188300920609     C                   MOVEL     *BLANKS       VI2DCT
188400940712     C                   Z-ADD     0             VI2SGL
188500940712     C                   Z-ADD     0             VI2ITR
188600940712     C                   Z-ADD     0             VI2MIN
188700940713     C                   Z-ADD     0             VI2MAX
188800920611     C                   MOVEL     ' '           VI2ATB
188900920609     C* 06 ON - TIPO CONSEGNA = I
189000920610     C   06              MOVEL     'S'           VI2AIN
189100920612     C                   MOVEL     *BLANKS       VI2ORP
189200940712     C                   Z-ADD     0             SAVITR
189300960520     C                   Z-ADD     0             SA2ITR
189400940712     C                   Z-ADD     0             SAVMIN
189500940713     C                   Z-ADD     0             SAVMAX
189600930219     C   06              MOVEL     'S'           SAVAIN
189700930219     C  N06              MOVEL     ' '           SAVAIN
189800920609     C                   Z-ADD     0             VI2NRR
189900941116     C*
190000920609     C                   ADD       1             NR2
190100920609     C                   ADD       1             B
190200941111     C                   WRITE     TA67S07
190300920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
190400920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
190500920619     C                   SETOFF                                       0336
190600920609     C                   END
190700920609     C*
190800920609     C                   Z-ADD     NR2           SAVNR2
190900920609     C                   Z-ADD     NR2           REC2
191000920609     C                   ENDSR
191100920611     C*
191200920611     C*--- ANNULLAMENTO TOTALE TARIFFA -------------------------------*
191300920611     C     ANNTAR        BEGSR
191400920709     C                   SETOFF                                       32
191500920611     C* ANNULLO DETTAGLIO
191600061011     C  n12KTPT          SETLL     TITPD000
191700061011     c   12ktpt          setll     tiopd000
191800920611     C     *IN32         DOWEQ     '0'
191900061011     C  n12KTPT          READE     TITPD000                               32
192000061011     c   12ktpt          reade     tiopd000                               32
192100940713     C  N32TPDATB        IFEQ      ' '
192200941109     C     *IN12         IFEQ      *ON
192300941109     C* OFFERTA
192400061011     C                   DELETE    tiopd000
192500941109     C                   ELSE
192600941109     C* TARIFFA
192700941109     C                   MOVEL     'A'           TPDATB
192800941109     C                   MOVEL     ' '           TPDFTR
192900040915     C                   Z-ADD     dateu8        TPDDTR
193000990614     C                   UPDATE    TITPD000
193100941109     C                   END
193200921217     C                   END
193300920611     C                   END
193400921130     C*
193500940713     C* ANNULLO TESTATA
193600061011     C  n12KTPT          CHAIN     TITPT000                           31
193700061011     c   12ktpt          chain     tiopt000                           31
193800940713     C     *IN31         IFEQ      *OFF
193900941109     C     *IN12         IFEQ      *ON
194000941109     C* OFFERTA
194100061011     C                   DELETE    tiopt000
194200941109     C                   ELSE
194300941109     C* TARIFFA
194400990615     C* DATA ULTIMA VARIAZIONE
194500990615     C                   Z-ADD     DATEU8        TPTDUV
194600941109     C                   MOVEL     'A'           TPTATB
194700941109     C                   MOVEL     ' '           TPTFTR
194800040915     C                   Z-ADD     dateu8        TPTDTR
194900990614     C                   UPDATE    TITPT000
195000030715      * mi salvo la tariffa particolare annullata
195100030715     c                   movel(p)  TPTftc        w_manftc
195200030715     c                   move      'A'           W_manftc
195300030715     c                   z-add     1             WW
195400030715     c     w_manftc      lookup    manftc                                 87
195500030715     c                   if        not %found
195600030715     c     '   '         lookup    manftc(ww)                             87
195700030715     c                   if        %found
195800030715     c                   movea     w_manftc      manftc(ww)
195900030715     c                   endif
196000030715     c                   endif
196100941109     C                   END
196200940713     C                   END
196300921130     C*
196400921217     C*
196500920618     C                   SETON                                        09
196600920611     C                   ENDSR
196700920609     C*
196800920609     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
196900920609     C     CONSFL        BEGSR
197000920619     C                   SETOFF                                       993304
197100070226     c                   setoff                                       7976
197200920609     C                   Z-ADD     1             NR2
197300031003     C* INDICATORE 62 -> RIMANE ACCESO SE NON CI SONO RECORD VALORIZZATI
197400031003     C*                  NEL SUBFILE
197500031003     C                   SETON                                        62
197600920609     C*
197700941111     C     NR2           CHAIN     TA67S07                            33
197800941111     C*
197900941111    1C     *IN33         DOWEQ     '0'
198000920624     C   15              MOVEL     DUPCTS        VI2CTS
198100940712     C   16              Z-ADD     DUPSGL        VI2SGL
198200940712     C   17              Z-ADD     DUPITR        VI2ITR
198300940712     C   18              Z-ADD     DUPMIN        VI2MIN
198400940713     C   23              Z-ADD     DUPMAX        VI2MAX
198500920624     C   19              MOVEL     DUPAIN        VI2AIN
198600920616     C*
198700941111    2C     VI2CTS        IFNE      *BLANKS
198800940713     C     RECORD        ORNE      *BLANKS
198900940713     C     RECORD        ANDNE     *ZEROS
199000031003      *
199100031003     C                   SETOFF                                       62
199200941111     C*
199300920609     C* CONTROLLO SE RECORD DI NUOVA IMMISSIONE
199400941111    3C     VI2NRR        IFEQ      0
199500920609     C                   SETOFF                                       05
199600920827     C*
199700920827     C*** CODICE TASSAZIONE
199800920609     C* CODICE TASSAZIONE OBBLIGATORIO
199900941111    4C     VI2CTS        IFEQ      *BLANKS
200000920611     C                   SETON                                        4599
200100920611     C                   GOTO      ENDSFL
200200941111    4C                   END
200300010920     C* CODICE TASSAZIONE "EE" ERRATO
200400020328      * se tariffa DPD
200500010920    4C     VI2CTS        IFEQ      'EE'
200600020325     C     FLGNET        ANDEQ     'D'
200700011022     C                   SETON                                        741399
200800011022     C                   MOVEA     MSG(1)        VI2MSG
200900011022     C                   GOTO      ENDSFL
201000010920    4C                   ENDIF
201100020328
201200020328      * se tariffa italia non accetto codice tassazione 'EE'
201300020328     c                   if        parfie = 'I' and vi2cts = 'EE'
201400020328     c                   eval      *in74 = *on
201500020328     c                   eval      *in13 = *on
201600020328     c                   eval      *in99 = *on
201700020328     c                   eval      vi2msg = msg(1)
201800020328     c                   goto      endsfl
201900020328     c                   endif
202000020328      * se tariffa estera non accetto codice tassazione 'IT'
202100020328     c                   if        parfie = 'E' and vi2cts = 'IT'
202200020328     c                   eval      *in74 = *on
202300020328     c                   eval      *in13 = *on
202400020328     c                   eval      *in99 = *on
202500020328     c                   eval      vi2msg = msg(4)
202600020328     c                   goto      endsfl
202700020328     c                   endif
202800020328
202900941020     C* SE LA CONSEGNA PARTICOLARE IN GESTIONE RICHIEDE UN PARTICOLARE
203000941111     C*   CODICE DI TASSAZIONE E' NECESSARIO CHE SIA PRESENTE SOLO
203100941020     C*   QUELLO NELLE RIGHE DI DETTAGLIO
203200950119    4C     WCTS          IFNE      *BLANKS
203300950119     C     WCTE          ANDNE     *BLANKS
203400941123     C*
203500950119    5C     WCTS          IFNE      VI2CTS
203600950119     C     WCTE          ANDNE     VI2CTS
203700950119     C                   MOVEA     WCTS          MS2(49)
203800950119     C                   MOVEA     WCTE          MS2(56)
203900950119     C                   MOVEA     MS2           VI2MSG
204000950119     C                   SETON                                        741399
204100941020     C                   GOTO      ENDSFL
204200941123    5C                   ENDIF
204300941123     C*
204400941123   X4C                   ELSE
204500941123     C*
204600941123    5C     WCTS          IFNE      *BLANKS
204700941123     C     WCTS          ANDNE     VI2CTS
204800950119     C                   MOVEA     WCTS          MS1(50)
204900950119     C                   MOVEA     MS1           VI2MSG
205000950119     C                   SETON                                        741399
205100941123     C                   GOTO      ENDSFL
205200941123    5C                   ENDIF
205300950119     C*
205400950119    5C     WCTE          IFNE      *BLANKS
205500950119     C     WCTE          ANDNE     VI2CTS
205600950119     C                   MOVEA     WCTE          MS1(50)
205700950119     C                   MOVEA     MS1           VI2MSG
205800950119     C                   SETON                                        741399
205900950119     C                   GOTO      ENDSFL
206000950119    5C                   ENDIF
206100941123    4C                   ENDIF
206200120713
206300120713      * ricerca codice tassazione
206400120713     c                   IF        %scan('?':VI2cts) > 0
206500120713     c                   clear                   TRTB09DS
206600120713     c                   eval      TB09fun = '1'
206700120713     c                   eval      TB09ord = '1'
206800120713     c                   IF        flgnet = 'D'
206900120713     c                   eval      TB09est = 'E'
207000120713     c                   ELSE
207100120713     c                   eval      TB09est = parfie
207200120713     c                   ENDIF
207300120713     c                   eval      TB09uta = 'S'
207400120713     c                   IF        flgnet = 'F'
207500120713     c                   eval      TB09utf = 'S'
207600120713     c                   ELSE
207700120713     c                   eval      TB09utf = 'N'
207800120713     c                   ENDIF
207900120713     c                   eval      kpjbu = TRTB09DS
208000120713     c                   call      'TRTB09R'
208100120713     c                   parm                    kpjba
208200120713     c                   eval      TRTB09DS = kpjbu
208300120713     c                   IF        TB09cod <> *blanks
208400120713     c                   eval      VI2cts = TB09cod
208500120713     c                   ELSE
208600120723     c                   clear                   VI2cts
208700120713     c                   ENDIF
208800120713     c                   ENDIF
208900941123     C*
209000920609     C* SE IMMESSO COD.TASSAZIONE CONTROLLO SE ESISTE
209100920609     C                   MOVEL     'CT'          COD
209200920611     C                   MOVEL     *BLANKS       KEY
209300920609     C                   MOVEL     VI2CTS        KEY
209400920609     C     KTAB          CHAIN     TABEL                              34
209500920611     C* CONTROLLO VALIDITA' CODICE TASSAZIONE
209600941111    4C  N34TBLFLG        IFNE      *BLANKS
209700920611     C                   SETON                                        4699
209800920611     C                   GOTO      ENDSFL
209900941111   X4C                   ELSE
210000950112     C                   MOVEL     TBLUNI        DSCT
210100950112     C*
210200950112     C*
210300920611     C* DECODIFICO CODICE TASSAZIONE
210400920617     C                   MOVEL     §CTDES        VI2DCT
210500990614     C                   MOVE      §CTCOR        VI2ORP
210600941111    4C                   END
210700950112     C*
210800920611     C* 34 ON - CODICE TASSAZIONE INESISTENTE
210900920611     C   34              SETON                                        4799
211000920611     C   34              GOTO      ENDSFL
211100050304      * errore bloccante se codice tassazione non utilizzabile in gestione
211200050304      * tariffe clienti
211300050304     c                   If        §ctuta = 'N'
211400050304     c                   Eval      *In47 = *On
211500050304     c                   Eval      *In99 = *On
211600050304     c                   Goto      endsfl
211700050304     c                   EndIf
211800020207      *
211900020207      * Se è tariffa FedEx devo copiare solo i codici tassazioni
212000020207      * previsti per FedEx
212100020207     C     FLGNET        IFEQ      'F'
212200020207     C     §CTUTF        ANDNE     'S'
212300020207     C                   SETON                                        769913
212400020207     C                   MOVEA     MSG(2)        VI2MSG
212500020207     C                   GOTO      ENDSFL
212600020207     C                   ENDIF
212700020207      * Se NON è tariffa FedEx devo copiare solo i codici tassazioni
212800020207      * NON previsti per FedEx
212900020207     C     FLGNET        IFNE      'F'
213000020207     C     §CTUTF        ANDEQ     'S'
213100020207     C                   SETON                                        779913
213200020207     C                   MOVEA     MSG(3)        VI2MSG
213300020207     C                   GOTO      ENDSFL
213400020207     C                   ENDIF
213500070226      * Se tariffa estero accetto solo estero
213600070226     c                   if        parfie = 'E' and §ctest = *Blanks
213700070226     c                   eval      *in13 = *on
213800070226     c                   eval      *in76 = *on
213900070226     c                   eval      *in99 = *on
214000070226     c                   movea     msg(12)       vi2msg
214100070226     c                   goto      endsfl
214200070226     c                   endif
214300070226      * Se tariffa DPD accetto solo estero
214400070226      * tranne il codice generico IT xchè va bene
214500070226     c                   if        flgnet = 'D' and §ctest = *Blanks and
214600070227     c                             vi2cts <> 'IT'
214700070226     c                   eval      *in13 = *on
214800070226     c                   eval      *in76 = *on
214900070226     c                   eval      *in99 = *on
215000070226     c                   movea     msg(12)       vi2msg
215100070226     c                   goto      endsfl
215200070226     c                   endif
215300070226      * Se tariffa italia accetto solo italia
215400070226     c                   if        parfie = 'I' and flgnet <> 'D' and
215500070226     c                                              §ctest <> *Blanks
215600070226     c                   eval      *in13 = *on
215700070226     c                   eval      *in76 = *on
215800070226     c                   eval      *in99 = *on
215900070226     c                   movea     msg(12)       vi2msg
216000070226     c                   goto      endsfl
216100070226     c                   endif
216200050704
216300050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
216400050704      * per codice di tassazione
216500050704     c                   If        vi2tpg = '2' and vi2atb = *Blanks
216600061011     c  n12ktpd01        Setll     titpd01l
216700061011     c   12ktpd01        Setll     tiopd01l
216800050704     c                   Do        *Hival
216900061011     c  n12ktpd01        Reade(n)  titpd01l
217000061011     c   12ktpd01        Reade(n)  tiopd01l
217100061011     c                   If        %eof
217200050704     c                   Leave
217300050704     c                   EndIf
217400050704     c                   If        tpdatb <> *Blanks
217500050704     c                   Iter
217600050704     c                   EndIf
217700050704     c                   Eval      *in74 = *on
217800050704     c                   Eval      *in13 = *on
217900050704     c                   Eval      *in99 = *on
218000050704     c                   Eval      vi2msg = msg(6)
218100050704     c                   Goto      endsfl
218200050704     c                   EndDo
218300050704     c                   EndIf
218400020207      *
218500920827     C*
218600920827     C*** SCAGLIONE OBBLIGATORIO
218700941111    4C     VI2SGL        IFEQ      0
218800920609     C                   SETON                                        4899
218900920609     C                   GOTO      ENDSFL
219000950119   X4C                   ELSE
219100990622     C* SE TIPO TARIFFA/PAGAMENTO E' A VALORE LO SCAGLIONE PUO' ESSERE UN VALORE
219200990622     C* ANCHE CON I DECIMALI IN BASE ALLA DIVISA
219300990622    5C     *IN14         IFEQ      *ON
219400990622     C                   MOVE      VI2SGL        W0030
219500990622    6C     W0030         IFGT      0
219600990622     C     FLGDEC        ANDNE     'S'
219700990622     C                   SETON                                        7199
219800990622     C                   GOTO      ENDSFL
219900990622    6C                   ENDIF
220000990622     C**
220100990622   X5C                   ELSE
220200950119     C* WSCD = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
220300950119     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
220400990622    6C     WSCD          IFEQ      ' '
220500950119     C                   MOVE      VI2SGL        W0030
220600950119     C     W0030         IFGT      0
220700950119     C                   SETON                                        7199
220800950119     C                   GOTO      ENDSFL
220900950119     C                   ENDIF
221000990622    6C                   ENDIF
221100990622    5C                   ENDIF
221200080908      * se non ci sono errori e la tariffa particolare è fuel e non sono
221300080908      * utente edp non posso inserire uno scaglione minore di 999
221400080922      * OBSOLETO
221500080922    5c***                If        vidftc = 'f ' and
221600080922     c***                          %subst(knmus: 1: 3) <> 'EDP' and
221700080922     c***                          vi2sgl < 999
221800080922     c****               seton                                        8899
221900080922     c****               goto      endsfl
222000080922     c*****              endif
222100080922    4C                   END
222200920827     C*
222300920827     C*** CONTROLLO SE RIGHE DOPPIE
222400061011     C  n12KTPD          CHAIN     TITPD000                           32
222500061011     c   12ktpd          chain     tiopd000                           32
222600061011     c  n32
222700061011    4CanN12TPDNRR        IFNE      VI2NRR
222800941111     C     TPDATB        ANDEQ     ' '
222900920709     C                   SETON                                        4999
223000920709     C                   GOTO      ENDSFL
223100941111    4C                   END
223200061011     c  n32
223300061011    4can 12tponrr        ifne      VI2NRR
223400061011     C     TPDATB        ANDEQ     ' '
223500061011     C                   SETON                                        4999
223600061011     C                   GOTO      ENDSFL
223700061011    4C                   END
223800920828     C*
223900941111   X3C                   ELSE
224000920617     C                   SETON                                        05
224100941111    3C                   END
224200060307
224300060307      * se tariffa particolare 'd ' AC BASE non posso andare al di sotto del
224400060307      * minimo indicato in DV 1463
224500060705      * o in tabella FAB
224600060313     c                   If        vidftc = 'd '
224700060705      * controllo se il cliente è presente sulla tabella della forzatura
224800060705      * AC BASE e controllo la tariffa inserita con quanto presente in tabella
224900060705     c                   clear                   dsbs02
225000060705     c                   clear                   dfab
225100060705     c                   eval      t02mod = 'C'
225200060705     c                   eval      t02sif = knsif
225300060705     c                   eval      t02cod = 'FAB'
225400060705     c  n12              movel     vidcli        t02ke1
225500060705     c   12              movel     visksc        t02ke1
225600060705     c                   call      'TIBS02R'
225700060705     c                   parm                    kpjba
225800060705     c                   parm                    dsbs02
225900060705     c                   if        t02err = *blanks
226000060705     c                   eval      dfab = t02uni
226100060705     c                   endif
226200060705     c                   if        vi2itr < d§fabitr
226300060705     c                   eval      vi2msg = msg(11)
226400060705     c                   eval      %subst(vi2msg:38:12) = %editc(d§fabitr:'4')
226500060705     c                   eval      *in13 = *on
226600060705     c                   eval      *in79 = *on
226700060705     c                   eval      *in99 = *on
226800060705     c                   goto      endsfl
226900060705     c                   endif
227000060705      * se cliente presente in tabella FAB non faccio gli altri controlli
227100060705     c                   if        d§fabitr = *zeros
227200140127
227300140127       //?Recupero la tariffa AC BASE dal file TIACB
227400140127     c     kACB          setll     TIACB01L
227500140127     c     CLPclv        reade     TIACB01L
227600140127     c                   IF        %found(TIACB01L) and
227700140127     c                             VI2itr < ACBitr
227800140127     c                   eval      vi2msg = msg(7)
227900140127     c                   eval      %subst(vi2msg:10:1) = clpclv
228000140127     c                   eval      VI2msg = %trim(VI2msg) + ' ' +
228100140127     c                             %trim(%editc(ACBitr:'4'))
228200140127     c                   eval      *in13 = *on
228300140127     c                   eval      *in79 = *on
228400140127     c                   eval      *in99 = *on
228500140127     c                   goto      endsfl
228600140127     c                   ENDIF
228700140127
228800140127     c******             Select
228900060307      * clienti Top o Direzionali
229000140127     c******             when      (clpclv = 'T' or clpclv = 'D') and
229100140116     c*****                        vi2itr < 0,003
229200111130     c****                         vi2itr < 0,002
229300140127     c*****              eval      vi2msg = msg(7)
229400140116     c*****              eval      %subst(vi2msg:50:5) = '0,003'
229500111130     c****               eval      %subst(vi2msg:50:5) = '0,002'
229600140127     c******             eval      %subst(vi2msg:10:1) = clpclv
229700140127     c******             eval      *in13 = *on
229800140127     c******             eval      *in79 = *on
229900140127     c******             eval      *in99 = *on
230000140127     c******             goto      endsfl
230100060307      * clienti A
230200140127     c******             when      clpclv = 'A' and
230300140116     c*****                        vi2itr < 0,004
230400111130     c****                         vi2itr < 0,003
230500140127     c******             eval      vi2msg = msg(7)
230600140116     c*****              eval      %subst(vi2msg:50:5) = '0,004'
230700111130     c****               eval      %subst(vi2msg:50:5) = '0,003'
230800140127     c******             eval      %subst(vi2msg:10:1) = clpclv
230900140127     c******             eval      *in13 = *on
231000140127     c******             eval      *in79 = *on
231100140127     c******             eval      *in99 = *on
231200140127     c******             goto      endsfl
231300060307      * clienti C o B
231400140127     c******             when      (clpclv = 'C' or clpclv = 'B') and
231500140116     c*****                        vi2itr < 0,007
231600111130     c****                         vi2itr < 0,005
231700140127     c******             eval      vi2msg = msg(7)
231800140116     c*****              eval      %subst(vi2msg:50:5) = '0,007'
231900111130     c****               eval      %subst(vi2msg:50:5) = '0,005'
232000140127     c******             eval      %subst(vi2msg:10:1) = clpclv
232100140127     c******             eval      *in13 = *on
232200140127     c******             eval      *in79 = *on
232300140127     c******             eval      *in99 = *on
232400140127     c******             goto      endsfl
232500140127     c******             endsl
232600060705     c                   endif
232700060313     c                   endif
232800920827     C*
232900930219     C*** PER TUTTE CONSEGNE PARTICOLARI SI PUO' ACCETTARE
233000921202     C*     L'IMPORTO TARIFFA = 0  CON IL CMD14 DI FORZATURA
233100941111    3C     VI2ITR        IFEQ      0
233200930219     C  NKN              SETON                                        6199
233300921202     C   99              GOTO      ENDSFL
233400960520     C*
233500941111   X3C                   ELSE
233600960520     C* CONTROLLO CHE NON SUPERI LA MASSIMA % PER LA TARIFFA A VALORE:
233700960520     C*   SE LA SUPERA EMETTO MESSAGGIO FORZABILE
233800941111    4C     *IN14         IFEQ      *ON
233900960520     C*
234000960704    5C     VI2ITR        IFNE      SA2ITR
234100960520     C     VI2ITR        ANDGT     WPTV
234200941110     C                   SETON                                        6699
234300960704     C                   MOVEL     VI2ITR        SA2ITR
234400941110     C                   GOTO      ENDSFL
234500941111    5C                   END
234600941111     C*
234700941111   X4C                   ELSE
234800941110     C* SE SI TRATTA DI TARIFFA IN £ NON SI POSSONO INSERIRE I DECIMALI
234900941111     C                   MOVE      VI2ITR        W0030             3 0
235000941111    5C     W0030         IFGT      0
235100990622     C     FLGDEC        ANDNE     'S'
235200941110     C                   SETON                                        6899
235300941110     C                   GOTO      ENDSFL
235400941111    5C                   END
235500941111    4C                   END
235600941111    3C                   END
235700990622     C*
235800990622     C*** IMPORTO MINIMO  TARIFFA
235900990622     C* CONTROLLO I DECIMALI
236000990622    3C     VI2MIN        IFGT      0
236100990622     C                   MOVE      VI2MIN        W0030             3 0
236200990622    4C     W0030         IFGT      0
236300990622     C     FLGDEC        ANDNE     'S'
236400990622     C                   SETON                                        6499
236500990622     C                   GOTO      ENDSFL
236600990622    4C                   END
236700990622    3C                   END
236800990622     C*
236900990622     C*** IMPORTO MASSIMO TARIFFA
237000990622     C* CONTROLLO I DECIMALI
237100990622    3C     VI2MAX        IFGT      0
237200990622     C                   MOVE      VI2MAX        W0030             3 0
237300990622    4C     W0030         IFGT      0
237400990622     C     FLGDEC        ANDNE     'S'
237500990622     C                   SETON                                        6599
237600990622     C                   GOTO      ENDSFL
237700990622    4C                   END
237800990622    3C                   END
237900920827     C*
238000940713     C*** IMPORTO MASSIMO TARIFFA
238100940713     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
238200941111    3C     VI2MAX        IFGT      0
238300940713     C     VI2MAX        ANDLT     VI2MIN
238400940713     C                   SETON                                        6399
238500940713     C                   GOTO      ENDSFL
238600941111    3C                   END
238700940713     C*
238800920616     C* SE NON CI SONO ERRORI AGGIORNO E VADO AVANTI A LEGGERE
238900920616     C                   MOVEL     VI2CTS        DUPCTS
239000940712     C                   Z-ADD     VI2SGL        DUPSGL
239100941110     C                   Z-ADD     VI2ITR        DUPITR
239200940712     C                   Z-ADD     VI2MIN        DUPMIN
239300940713     C                   Z-ADD     VI2MAX        DUPMAX
239400920616     C   06              MOVEL     VI2AIN        DUPAIN
239500941111     C*
239600941111   X2C                   ELSE
239700930225     C                   SETOFF                                       05
239800941111    2C                   END
239900920616     C*
240000920611     C     ENDSFL        TAG
240100920610     C   99              Z-ADD     NR2           REC2
240200920619     C*
240300941111     C                   UPDATE    TA67S07
240400920619     C   99              SETON                                        33
240500920619     C*
240600921126     C                   ADD       1             NR2
240700941111     C  N99NR2           CHAIN     TA67S07                            33
240800941111    1C                   END
240900031003      * SE E' ACCESO L'INDICATORE 62 SEGNALO ERRORE CHE E' OBBLIGATORIO IL
241000031003      * DETTAGLIO TARIFFA
241100031003     C                   IF        *IN62
241200031003     C                   SETON                                        99
241300031003     C                   ENDIF
241400920611     C                   ENDSR
241500920610     C*
241600920611     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
241700920611     C     CONCTL        BEGSR
241800060802     C                   SETOFF                                       999886
241900921126     C* AZZERO CAMPI DI DUP
242000940712     C                   MOVEL     *BLANKS       DUPCTS
242100940712     C                   Z-ADD     0             DUPSGL
242200941110     C                   Z-ADD     0             DUPITR
242300940712     C                   Z-ADD     0             DUPMIN
242400940713     C                   Z-ADD     0             DUPMAX
242500940712     C   06              MOVEL     *BLANKS       DUPAIN
242600060307
242700060307      * faccio subito i controlli per AC BASE e AC PLUS
242800060307
242900060307      * se tariffa particolare "d " AC BASE la tariffa non deve
243000060307      * avere il flag di rinuncia AC BASE
243100060307     c                   if        vidftc = 'd ' and parfmp = 'S'
243200060307     c                   eval      vi2msg = msg(8)
243300060307     c                   eval      *in13 = *on
243400060307     c                   eval      *in99 = *on
243500060307     c                   leavesr
243600060307     c                   endif
243700060307
243800060307      * se tariffa particolare "d " AC BASE la tariffa non deve
243900060307      * avere una tariffa particolare AC PLUS reale con applicazione completa
244000060307     c                   if        vidftc = 'd '
244100060307     c                   movel     'C '          v09ftc
244200061011     c  n12ktpt9         chain(n)  titpt01l
244300061011     c   12ktpt9         chain(n)  tiopt01l
244400061011     c                   if        %found and tptatb = *blanks and
244500060307     c                             tpttma = *blanks and tptapl = *blanks
244600060307     c                   eval      vi2msg = msg(9)
244700060307     c                   eval      *in13 = *on
244800060307     c                   eval      *in99 = *on
244900060307     c                   leavesr
245000060307     c                   endif
245100060307     c                   endif
245200060307
245300060307      * se tariffa particolare "C " AC PLUS reale completa la
245400060307      * tariffa non deve avere una tariffa particolare AC BASE
245500060307     c                   if        vidftc = 'C ' and vi2mas = *blanks and
245600060307     c                             vi2apl = *blanks
245700060307     c                   movel     'd '          v09ftc
245800061011     c  n12ktpt9         chain(n)  titpt01l
245900061011     c   12ktpt9         chain(n)  tiopt01l
246000061011     c                   if        %found and tptatb = *blanks
246100060307     c                   eval      vi2msg = msg(10)
246200060307     c                   eval      *in13 = *on
246300060307     c                   eval      *in99 = *on
246400060307     c                   leavesr
246500060307     c                   endif
246600060307     c                   endif
246700920827     C*
246800920924     C*** ARROTONDAMENTI : TUTTI <> 0  OPPURE  TUTTI = 0
246900941111    1C     VI2ARL        IFEQ      0
247000920611     C*
247100941111    2C     VI2ARF        IFNE      0
247200940712     C     VI2ARO        ORNE      0
247300920611     C                   SETON                                        4299
247400920611     C                   GOTO      ENDCON
247500941111    2C                   END
247600920611     C*
247700941111   X1C                   ELSE
247800940712     C*
247900941111    2C     VI2ARF        IFEQ      0
248000940712     C     VI2ARO        OREQ      0
248100920611     C                   SETON                                        4299
248200920611     C                   GOTO      ENDCON
248300941111    2C                   END
248400920611     C*
248500941111    1C                   END
248600920624     C*
248700950119     C* SE SONO STATI IMMESSI GLI ARROTONDAMENTI CONTROLLO SE SI TRATTA
248800950119     C*   DI SCAGLIONE SENZA DECIMALI PER CUI NON SI POSSONO INSERIRE
248900950119     C*   VALORI DOPO LA VIRGOLA
249000950119    1C     VI2ARL        IFGT      0
249100950119     C     WSCD          ANDEQ     ' '
249200950119     C                   MOVE      VI2ARL        W0030
249300950119    2C     W0030         IFGT      0
249400950119     C                   SETON                                        7299
249500950119     C                   GOTO      ENDCON
249600950119    2C                   ENDIF
249700950119     C*
249800950119     C                   MOVE      VI2ARF        W0030
249900950119    2C     W0030         IFGT      0
250000950119     C                   SETON                                        7299
250100950119     C                   GOTO      ENDCON
250200950119    2C                   ENDIF
250300950119     C*
250400950119     C                   MOVE      VI2ARO        W0030
250500950119    2C     W0030         IFGT      0
250600950119     C                   SETON                                        7299
250700950119     C                   GOTO      ENDCON
250800950119    2C                   ENDIF
250900950119    1C                   ENDIF
251000950119     C*
251100941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
251200941111    1C     *IN11         IFEQ      '1'
251300930223     C*
251400930223     C*** VALORE MERCE FORZABILE A ZERO CON ENTER
251500980309     C* SOLO SE NON SI TRATTA DI UNA TARIFFA IMPORT
251600990217    2C     *IN43         IFEQ      *ON
251700980429     C** SE SI TRATTA DI UNA MANDATO ASSICURATIVO FITTIZIO VI2MAS = N
251800980429     C** IL VALORE MERCE ASSICURABILE DEVE ESSERE A ZERO
251900981126    3C     VI2MAS        IFEQ      'F'
252000980429     C     VI2VLM        ANDGT     0
252100980429     C                   SETON                                        5299
252200980429     C                   GOTO      ENDCON
252300981126    3C                   END
252400981126    2C                   END
252500980309     C*
252600981126    2C     IMPORT        IFNE      'Y'
252700980416     C* CONTROLLO SE TARIFFA ESTERA
252800981126     C   24
252900981126    3COR 25PARFIE        IFEQ      'E'
253000980416     C* SE IL VALORE MERCE E' A ZERO IL TIPO APPLICAZIONE DEVE ESSERE SOLO
253100980416     C* IN PARTENZA
253200980416     C     VI2VLM        ANDEQ     0
253300980416     C     VI2APL        ANDNE     'P'
253400980416     C                   SETON                                        5399
253500980416     C                   GOTO      ENDCON
253600981126    3C                   END
253700980416     C*
253800981126    3C   24UNO           IFEQ      ' '
253900930223     C     VI2VLM        ANDEQ     0
254000980505     C     VI2MAS        ANDEQ     ' '
254100930223     C                   SETON                                        5099
254200930223     C                   MOVEL     '1'           UNO
254300930223     C                   GOTO      ENDCON
254400981126    3C                   END
254500981126   X2C                   ELSE
254600980309     C* VALORE MERCE
254700981126    3C   24VI2VLM        IFEQ      *ZEROS
254800980309     C                   SETON                                        5599
254900981126    3C                   END
255000980309     C* APPLICAZIONE TARIFFA SOLO IN ARRIVO
255100981126     C   24
255200981126    3COR 25VI2APL        IFNE      'A'
255300980309     C                   SETON                                        5499
255400980416     C                   GOTO      ENDCON
255500981126    3C                   END
255600980309     C*
255700981126    2C                   END
255800990817     C* VALORE MERCE VERIFICO SE HA IMPORTI CON DECIMALI
255900990817     C*
256000990817    2C     VI2VLM        IFGT      0
256100990817     C                   MOVE      VI2VLM        W0030             3 0
256200990817    3C     W0030         IFGT      0
256300990817     C     FLGDEC        ANDNE     'S'
256400990817     C                   SETON                                        6799
256500990817     C                   GOTO      ENDCON
256600990817    3C                   END
256700990817    2C                   END
256800921009     C*
256900930219     C*** LIMITE RISARCIBILE NON PUO' ESSERE MINORE DI 12000
257000990907    2C   25VI2VLM        IFLT      §GELRP
257100930219     C                   SETON                                        5699
257200930219     C                   GOTO      ENDCON
257300941111    2C                   END
257400921009     C*
257500920827     C*** TIPO VALORE MERCE
257600920827     C* TIPO VALORE OBBLIGATORIO
257700941111    2C     VI2FVM        IFEQ      '?'
257800980316     C                   MOVEL     'TR'          COD
257900920925     C                   MOVEL     ' '           VI2FVM
258000001020     C                   MOVE      *BLANKS       KEY
258100920925     C                   MOVEL     *BLANKS       DES
258200920925     C                   CALL      'X§TABER'
258300920925     C                   PARM                    CODUT
258400920925     C                   PARM                    COD
258500920925     C                   PARM                    KEY
258600920925     C                   PARM                    DES
258700920925     C                   MOVEL     KEY           VI2FVM
258800920925     C                   MOVEL     DES           DESFVM
258900920925     C                   SETON                                        98
259000920925     C                   GOTO      ENDCON
259100941111    2C                   END
259200920925     C*
259300941111    2C     VI2FVM        IFEQ      ' '
259400920827     C                   SETON                                        5799
259500920827     C                   GOTO      ENDCON
259600941111    2C                   END
259700941111     C*
259800920827     C* CONTROLLO VALIDITA' TIPO VALORE
259900980316     C                   MOVEL     'TR'          COD
260000920827     C                   MOVEL     *BLANKS       KEY
260100920827     C                   MOVEL     VI2FVM        KEY
260200920827     C     KTAB          CHAIN     TABEL                              34
260300920827     C*
260400920827     C* SE TIPO VALORE ANNULLATO : ERRORE
260500941111    2C  N34TBLFLG        IFNE      ' '
260600920827     C                   SETON                                        5899
260700920827     C                   GOTO      ENDCON
260800941111   X2C                   ELSE
260900980316     C                   MOVEL     TBLUNI        DSTR
261000941111     C* CONTROLLO SE E' DA UTILIZZARE PER IL CALCOLO VALORE
261100980316     C     §TRUCV        IFNE      'S'
261200920827     C                   SETON                                        5999
261300920827     C                   GOTO      ENDCON
261400920827     C                   ELSE
261500920827     C* DECODIFICO TIPO VALORE
261600980316     C                   MOVEL     §TRDES        DESFVM
261700920827     C                   END
261800941111    2C                   END
261900920827     C*
262000920827     C* 34 ON - TIPO VALORE MERCE INESISTENTE
262100920827     C   34              SETON                                        5999
262200060802     C                   GOTO      ENDCON
262300941111    1C                   END
262400060802
262500060802      * 82 ON - controllo la data riferimento prezzo base
262600060802     c                   if        *in82
262700060802      * la data non può essere a zero
262800060802     c                   if        vi2dpb = 0
262900060802     c                   seton                                        8399
263000060802     c                   goto      endcon
263100060802     c                   endif
263200060802      * verifico la correttezza della data
263300060802     c                   clear                   W_vi2dpb
263400060802     c                   clear                   wlbdat
263500060802     c                   move      VI2dpb        g08dat
263600060802     c                   Call      'XSRDA8'
263700060802     c                   parm                    Wlbdat
263800060802     c                   If        g08err = '1'
263900060802     c                   seton                                        8399
264000060802     c                   goto      endcon
264100060802     c                   else
264200060802     c                   movel     g08dat        VI2dpb
264300060802     c                   move      g08inv        W_vi2dpb
264400060802     c                   endif
264500061212      *? ATTENZIONE CHIODO!!!  ?
264600061212      * se data minore di 31/12/2006 errore
264700061212     c                   if        w_vi2dpb < 20061231
264800060803     c                   seton                                        8599
264900060803     c                   goto      endcon
265000060803     c                   endif
265100061207      * verifico che non sia più grande di 06 mesi da oggi
265200060802     c                   if        w_vi2dpb > Datamax
265300060802     c                   seton                                        8499
265400060802     c                   goto      endcon
265500060802     c                   endif
265600060802
265700060802     c                   endif
265800920827     C*
265900920611     C     ENDCON        ENDSR
266000921009     C*
266100920624     C*--- AGGIORNO CAMPI DEL SFL ------------------------------------*
266200920610     C     REGIS         BEGSR
266300920709     C*
266400920610     C                   Z-ADD     1             NR2
266500941111     C     NR2           CHAIN     TA67S07                            31
266600920610     C     *IN31         DOWEQ     '0'
266700920616     C     VI2CTS        IFNE      *BLANKS
266800921126     C*
266900921126     C                   SETOFF                                       60
267000921126     C* VI2NRR = 0  SI TRATTA DI RECORD DI NUOVA IMMISSIONE
267100921126     C     VI2NRR        IFEQ      0
267200921216     C                   SETON                                        60
267300921126     C*
267400921126     C                   ELSE
267500921126     C     VI2ATB        IFEQ      ' '
267600921126     C     VI2REC        IFNE      SAVREC
267700921216     C                   SETON                                        60
267800921126     C                   END
267900921126     C                   ELSE
268000921126     C                   SETON                                        60
268100921126     C                   END
268200921126     C                   END
268300921126     C*
268400061011     C   60
268500061011     Cann12KTPD          CHAIN     TITPD000                           33
268600061011     c   60
268700061011     can 12ktpd          chain     tiopd000                           33
268800921126     C   60VI2ATB        IFNE      ' '
268900030715     c                   if        *in33 = *off
269000030715      * non è offerta
269100030715     c                   if        *in12 = *off
269200030715     C                   MOVEL     'A'           TPDATB
269300030715     C                   MOVEL     ' '           TPDFTR
269400040915     C                   Z-ADD     dateu8        TPDDTR
269500030715     C                   UPDATE    TITPD000
269600030716      * verifico prima se esiste l'immissioine di questa tariffa
269700030716     c                   movel(p)  VIDftc        w_manftc
269800030716     c                   move      'I'           W_manftc
269900030716     c                   z-add     1             WW
270000030716     c     w_manftc      lookup    manftc                                 87
270100030715      * mi salvo la tariffa particolare da registrare nello storico variazioni
270200030716      * se non esiste la fase di immissione
270300030716     c                   if        not %found
270400030716      *
270500030715     c                   movel(p)  VIDftc        w_manftc
270600030715     c                   move      'M'           W_manftc
270700030715     c                   z-add     1             WW
270800030715     c     w_manftc      lookup    manftc                                 87
270900030715     c                   if        not %found
271000030715     c     '   '         lookup    manftc(ww)                             87
271100030715     c                   if        %found
271200030715     c                   movea     w_manftc      manftc(ww)
271300030715     c                   endif
271400030715     c                   endif
271500030716      *
271600030716     c                   endif
271700030715      *
271800030715     c                   else
271900030715      * offerta
272000061011     C                   DELETE    tiopd000
272100030715     c                   endif
272200921218     C*
272300030715     c                   endif
272400030715     C*
272500990615     C* AGGANCIO TNTPT PER AGGIORNARE ULTIMA DATA VARIAZIONE CON TRASMISSIONE
272600990615     C* DEL DATO
272700061011     C  n12KTPT          CHAIN     TITPT000                           32
272800061011     c   12ktpt          chain     tiopt000                           32
272900990615     C  N32              CLEAR                   TPTFTR
273000990615     C* DATA ULTIMA VARIAZIONE
273100990615     C  N32              Z-ADD     DATEU8        TPTDUV
273200040915     C  N32              z-add     dateu8        TPTDTR
273300061011     C  N32
273400061011     Cann12              UPDATE    TITPT000
273500061011     c  n32
273600061011     can 12              update    tiopt000
273700990615     C*
273800921218     C*
273900920610     C                   ELSE
274000920611     C* 33 ON  - IMMISSIONE
274100920611     C* 33 OFF - VARIAZIONE
274200941110     C                   Z-ADD     VI2ITR        TPDITR
274300940712     C                   Z-ADD     VI2MIN        TPDMIN
274400940713     C                   Z-ADD     VI2MAX        TPDMAX
274500930217     C*
274600930219     C   06VI2AIN        IFEQ      'N'
274700930219     C                   MOVEL     ' '           TPDAIN
274800930219     C                   ELSE
274900930219     C                   MOVEL     VI2AIN        TPDAIN
275000930219     C                   END
275100930217     C*
275200920611     C                   MOVEL     VI2ATB        TPDATB
275300920617     C                   MOVEL     VI2ORP        TPDORP
275400930217     C  N06              MOVEL     ' '           TPDAIN
275500921214     C                   MOVEL     ' '           TPDFTR
275600920611     C* SOLO PER IMMISSIONE
275700920611     C   33              DO
275800920611     C                   MOVEL     VI2CTS        TPDCTS
275900940712     C                   Z-ADD     VI2SGL        TPDSGL
276000920612     C                   Z-ADD     VIDCLI        TPDKSC
276100920612     C                   Z-ADD     VIDCTR        TPDCTR
276200920612     C                   Z-ADD     VIDPRG        TPDPRG
276300920612     C                   MOVEL     VIDFTC        TPDFTC
276400920610     C                   END
276500980220     C* VALORE POSITIVO NEI CAMPI
276600980220     C                   MLLZO     1             TPDSGL
276700980220     C                   MLLZO     1             TPDITR
276800980220     C                   MLLZO     1             TPDMIN
276900980220     C                   MLLZO     1             TPDMAX
277000920610     C*
277100040915     C                   Z-ADD     dateu8        TPDDTR
277200061011     c                   if        not *in12
277300061011     C  N33              UPDATE    TITPD000
277400061011     C   33              WRITE     TITPD000
277500061011     c                   endif
277600061011     c                   if        *in12
277700061011     c  n33              update    tiopd000
277800061011     c   33              write     tiopd000
277900061011     c                   endif
278000030715      *
278100921217     C*
278200990615     C* AGGANCIO TNTPT PER AGGIORNARE ULTIMA DATA VARIAZIONE CON TRASMISSIONE
278300990615     C* DEL DATO
278400061011     C  n12KTPT          CHAIN     TITPT000                           32
278500061011     c   12ktpt          chain     tiopt000                           32
278600030715     c                   if        *in32 = *off
278700030715     C                   CLEAR                   TPTFTR
278800040915     C                   Z-ADD     DATEU8        TPTDTR
278900990615     C* DATA ULTIMA VARIAZIONE
279000030715     C                   Z-ADD     DATEU8        TPTDUV
279100061011     C  n12              UPDATE    TITPT000
279200061011     c   12              update    tiopt000
279300030715      * mi salvo la tariffa particolare da registrare nello storico variazioni
279400030715      * se non sono in offerta
279500030715     c                   if        *in12 = *off
279600030715      *
279700030715     c                   movel(p)  VIDftc        w_manftc
279800030715     c                   move      'M'           W_manftc
279900030715     c                   z-add     1             WW
280000030715     c     w_manftc      lookup    manftc                                 87
280100030715     c                   if        not %found
280200030715     c     '   '         lookup    manftc(ww)                             87
280300030715     c                   if        %found
280400030715     c                   movea     w_manftc      manftc(ww)
280500030715     c                   endif
280600030715     c                   endif
280700030715     C*
280800030715     c                   endif
280900990615     C*
281000030715     c                   endif
281100921126     C*
281200920610     C                   END
281300920616     C                   END
281400920616     C                   ADD       1             NR2
281500941111     C     NR2           CHAIN     TA67S07                            31
281600920610     C                   END
281700920610     C                   ENDSR
281800920610     C*
281900920624     C*--- AGGIORNO CAMPI DEL SFLCTL ---------------------------------*
282000920624     C     REGIS2        BEGSR
282100920709     C*
282200920624     C* 32 ON  - IMMISSIONE
282300920624     C* 32 OFF - VARIAZIONE
282400061011     C  n12KTPT          CHAIN     TITPT000                           32
282500061011     c   12ktpt          chain     tiopt000                           32
282600920624     C                   MOVEL     VI2TPG        TPTTPG
282700940712     C                   Z-ADD     VI2ARL        TPTARL
282800920624     C                   Z-ADD     VI2ARF        TPTARF
282900920624     C                   Z-ADD     VI2ARO        TPTARO
283000920624     C                   Z-ADD     VI2RPV        TPTRPV
283100920709     C                   MOVEL     ' '           TPTATB
283200990915     C                   MOVEL     VI2MAS        TPTTMA
283300060802     c                   If        *in82
283400060802     C                   Z-ADD     W_VI2DPB      TPTDPB
283500060829     c                   else
283600060829     c                   clear                   tptdpb
283700060803     c                   endif
283800060803      *
283900060803     c   32              z-add     dateu8        TPTdit
284000941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
284100941111    1C     *IN11         IFEQ      *ON
284200941111     C                   Z-ADD     VI2VLM        TPTVLM
284300941111     C                   MOVEL     VI2FVM        TPTFVM
284400941111   X1C                   ELSE
284500941111     C                   Z-ADD     0             TPTVLM
284600941111     C                   MOVEL     ' '           TPTFVM
284700941111    1C                   ENDIF
284800941111     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
284900941111    1C     *IN28         IFEQ      *ON
285000941111     C                   MOVEL     VI2APL        TPTAPL
285100941111   X1C                   ELSE
285200941111     C                   MOVEL     ' '           TPTAPL
285300941111    1C                   ENDIF
285400941111     C*
285500920624     C* SOLO PER IMMISSIONE
285600941111    1C   32              DO
285700920624     C                   Z-ADD     VIDCLI        TPTKSC
285800920624     C                   Z-ADD     VIDCTR        TPTCTR
285900920624     C                   Z-ADD     VIDPRG        TPTPRG
286000920624     C                   MOVEL     VIDFTC        TPTFTC
286100941111    1C                   END
286200941111     C*
286300990614     C* SE VIENE VARIATO TITPT ABBLENCO FLAG E DATA TRASMISSIONE
286400941111    1C     COMTPT        IFNE      TPTDS
286500921215     C                   MOVEL     ' '           TPTFTR
286600040915     C                   Z-ADD     dateu8        TPTDTR
286700921217     C*
286800990903     C*
286900990615     C* DATA ULTIMA VARIAZIONE
287000990615     C                   Z-ADD     DATEU8        TPTDUV
287100941111    1C                   END
287200980220     C* VALORE POSITIVO NEI CAMPI
287300980220     C                   MLLZO     1             TPTARL
287400980220     C                   MLLZO     1             TPTARF
287500980220     C                   MLLZO     1             TPTARO
287600980220     C                   MLLZO     1             TPTRPV
287700980220     C                   MLLZO     1             TPTVLM
287800921215     C*
287900040915     C                   Z-ADD     dateu8        TPTDTR
288000061011     c                   if        not *in12
288100990614     C  N32              UPDATE    TITPT000
288200990614     C   32              WRITE     TITPT000
288300061011     c                   endif
288400061011     c                   if        *in12
288500061011     c  n32              update    tiopt000
288600061011     c   32              write     tiopt000
288700061011     c                   endif
288800030715      *
288900030715      * se non sono in offerta salvo il codice tariffa particolare  da scrivere
289000030715      * nello storico
289100030715      *
289200030715     c                   IF        *in12 = *off
289300030715      *
289400030715     c                   movel(p)  VIDftc        w_manftc
289500030715     c  n32              move      'M'           W_manftc
289600030715     c   32              move      'I'           W_manftc
289700030715     c                   z-add     1             WW
289800030715     c     w_manftc      lookup    manftc                                 87
289900030715     c                   if        not %found
290000030715     c     '   '         lookup    manftc(ww)                             87
290100030715     c                   if        %found
290200030715     c                   movea     w_manftc      manftc(ww)
290300030715     c                   endif
290400030715     c                   endif
290500921126     C*
290600030715     c                   endif
290700030715     c*
290800930318     C*
290900930318     C                   SETON                                        0207
291000930318     C                   SETOFF                                       01
291100930223     C*
291200930223     C                   MOVEL     VI2TPG        SAVTPG            1
291300930223     C                   Z-ADD     VI2VLM        SAVVLM            9 0
291400920624     C                   ENDSR
291500120320
291600120320      /free
291700120320       //--------------------------------------------------------------
291800120320       //?Visualizza Dati manca tariffa-
291900120320       //--------------------------------------------------------------
292000120320       BEGSR GesW01;
292100120320
292200120320         wFine = *off;
292300120320         W01dsp = ISB48dsp;
292400120320         W01tbl = ISB48tbl;
292500120320         W01lnp = ISB48lnp;
292600120320         W01lna = ISB48lna;
292700120320         W01pro = ISB48pro;
292800120320         W01cts = ISB48cts;
292900120320         W01ctr = ISB48ctr;
293000120320         W01ncl = ISB48ncl;
293100120320         W01pkf = ISB48pkf;
293200120320         W01vlf = ISB48vlf;
293300120320         W01err = ISB48err;
293400120320
293500120320         DOW wFine = *off;
293600120320           exfmt  TA67W01;
293700120320           //?- F12 = Ritorno
293800120320           IF  *inkl;
293900120320             wFine = *on;
294000120320           ENDIF;
294100120320         ENDDO;
294200120320
294300120320       ENDSR;
294400120320      /end-free
294500950119     C*---------------------------------------------------------------*
294600020207**  MS1
294700950119E' possibile inserire solo il Codice Tassazione "xx"                           1
294800950119**  MS2
294900950119E' possibile inserire solo i Codici Tassazione "xx" e "xx"                     1
295000011022**  MSG
295100050531Non è possibile inserire il Codice Tassazione "EE"                             1
295200050531Codice tassazione non utlizzabile per tariffa FedEx                            2
295300050531Codice tassazione utilizzabile solo per tariffe FedEx                          3
295400050531Non è possibile inserire il Codice Tassazione "IT"                             4
295500050531Impossibile inserire la Tar.Particolare "XX" che sarà eliminata il xx/xx/xx    5
295600050704Inserire solo uno scaglione per codice di tassazione!!                         6
295700140127Cliente " " l'importo non può essere inferiore a Euro                          7
295800060307Impossibile inserire AC BASE. Il cliente ha rinunciato al mandato ass. AC BASE 8
295900060307Impossibile inserire AC BASE. Presente AC PLUS reale con applicazione completa 9
296000060307Impossibile inserire AC PLUS reale con applicazione completa. Presente AC BASE 1
296100060705L'importo non può essere inferiore a xxxxxxxx,xxx Euro                        11
296200070226Codice tassazione non utilizzabile                                            12
