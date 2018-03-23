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
001700140128     fTIACB01L  if   e           k disk
001800131216     FTidpb01l  IF   E           K DISK
001900131216     FTipmg01l  IF   E           K DISK
002000920529     FTABEL00F  IF   E           K DISK
002100090916     FTivis05l  IF   E           K DISK    usropn
002200060306     fcnclp00f  if   e           k disk    usropn
002300060306     ftfclp00f  if   e           k disk    rename (cnclp000:tfclp000) usropn
002302140219      *
002400950112     D*
002500950119     D MS1             S              1    DIM(79) CTDATA PERRCD(79)
002600950119     D MS2             S              1    DIM(79) CTDATA PERRCD(79)
002700060705     D MSG             S             79    DIM(20) CTDATA PERRCD(1)
002800950112     D* SCHIERE RELATIVE ALLA TABELLA "TR"
002900990614     D WTT             S              2    DIM(50)
003000920609     D TTR             S              1    DIM(50)
003100980428     D DTR             S             10    DIM(50)
003200941110     D TTP             S              1    DIM(50)
003300950119     D TSP             S              1    DIM(50)
003400980429     D ARR             S              1    DIM(50)
003500980429     D RPV             S              1    DIM(50)
003600950112     D* SCHIERE RELATIVE ALLA TABELLA "15"
003700950112     D NAZ             S              3    DIM(20)
003800080609      * schiera filiali abilitate al lasciato avviso
003900080609     d sklav           s              3  0 dim(999)
004000080609
004100050531     D TParEli         S              2    DIM(20)
004200050531     D TParDat         S              8  0 DIM(20)
004300021209
004400030715     d MANftc          s              3    dim(150)
004500030715     d W_manftc        s              3
004600030715     d WW              s              3  0 inz
004700051012     d Kscfil          s              3  0
004800080609
004900080609     d xx              s              3  0
005000131218
005100131218     d wgio            s              5  0
005200060801
005300060801     D Wdataeur        S               D   DATFMT(*eur)
005400060801     D Wdataoggi       S               D   DATFMT(*eur)
005500060802     d Datamax         s              8  0
005600131218     d data_iso1       s               d   datfmt(*iso)
005700131218     d data_iso2       s               d   datfmt(*iso)
005800030715
005900021209     d Wfqt            s                   Like(§1pFqt)
006000050531     d
006100050531     d wdatadmy        s               d   datfmt(*dmy)
006200050704
006300050704     d savcts          s                   like(tpdcts)
006400061011     d codut           s              1  0 Inz(1)
006500060306     d kcc             s                   like(clpkcc) inz(151)
006600060306     d ksc             s                   like(clpksc)
006700021209
006900131218     d Kpmg            s                   like(TPTdpb)
007000120320
007100120320       // -?Flag booleani
007200120320     d wFine           s               n   inz(*off)
007300130125     d wBlocca         s               n   inz(*off)
007400060802
007500941020     D*
007600920612     D PARAM5          DS
007700941111     D* CODICE CLIENTE/NUMERO VISITA
007800920612     D  VIDCLI                 2      8  0
007900941111     D* RAGIONE SOCIALE
008000920612     D  RAGDES                 9     56
008100941111     D* PROGRESSIVO TARIFFA
008200920612     D  VIDPRG                57     59  0
008300941111     D* CODICE TARIFFA
008400920612     D  VIDCTR                60     62  0
008500941111     D* DECODIFICA CODICE TARIFFA
008600920612     D  VIDDCF                63     72
008700941111     D* DESCRIZIONE TARIFFA
008800920612     D  VIDDCV                73     87
008900941111     D* TESTATA PROGRAMMA
009000920612     D  VIDTES                88    121
009100941111     D* FLG = " " ---> TARIFFE     FLG <> " " ---> OFFERTE
009200920619     D  FLG                  122    122
009300941111     D* FLG2   --> SE RESTITUITO PIENO INDICA CHE E' STATO PREMUTO CMD3
009400921117     D  FLG2                 123    123
009500980309     D* FLAG ITALIA/ESTERO
009600980309     D  PARFIE               125    125
009700980429     D* FLAG TARIFFA IMPORT
009800980429     D  IMPORT               126    126
009900000420     D* FLAG CLIENTE POSTA
010000000420     D  WRKPT                127    127
010100990824     D* CODICE DELLA  DIVISA
010200990824     D  VICDIV               128    130
010300990622     D* FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
010400990824     D  FLGDEC               131    131
010500000915     D* DATA DECORRENZA TARIFFA
010600000915     D  PARDDT               132    139  0
010700000915     D* TIPO SEWRVIZIO BOLLA
010800000915     D  PARTSP               140    140
010900020201     D* Flag Network
011000020201     D  FLGNET               141    141
011100050531     D* DATA scadenza TaRIFFA alfanumerica
011200050531     D  PARDST               142    149
011300060306      * flag rinuncia AC Base
011400060306     d  parfmp               150    150
011500090916      * flag per richiamo da trattativa = 'T'
011600090916     d  partrat              151    151
011700941111     D*
011800920616     D                 DS
011900940712     D  VI2SGL                 1     13  3
012000941110     D  VI2ITR                14     24  3
012100990614     D  VI2MIN                25     35  3
012200990614     D  VI2MAX                36     46  3
012300990614     D  RECORD                 1     46
012400990614     D  VI2AIN                47     47
012500990614     D  VI2REC                14     47
012600921126     D                 DS
012700941110     D  SAVITR                 1     11  3
012800990614     D  SAVMIN                12     22  3
012900990614     D  SAVMAX                23     33  3
013000990614     D  SAVAIN                34     34
013100990614     D  SAVREC                 1     34
013200990615     D WLBDAT          DS
013300060802     D  G08DAT                 1      8  0
013400060802     D  G08INV                 9     16  0
013500060802     D  G08ERR                17     17
013600060802     D  G08TGI                18     22  0
013700920612     D KPJBA         E DS
013800920610     D TAR             DS
013900920610     D  TPDNRR               397    400B 0
014000061011     d taro            ds
014100061011     d  tponrr               397    400B 0
014200990614     D TPTDS         E DS                  EXTNAME(TITPT00F)
014300921215     D DS1P          E DS
014400020201     D DSSP          E DS
014500990907     D DGEI          E DS
014600920612     D DSCT          E DS
014700940913     D DSTR          E DS
014800950112     D DS15          E DS
014900060705     d dfab          e ds
014903140219      *
015000990217     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015100030718     D TNTA04DS      E DS
015200061011
015300061011     d Azuteds       e ds                  extname(Azute00f)
015400061011     d dDatiute      e ds
015500061011     d Tibs34ds      e ds
015600130125     d dUTE01        e ds
015700061011
015800061011     d Tnta25ds      e ds
015900061011     d Tnta25tad     e ds                  inz
016000061011     d Tnta25tgc     e ds                  inz
016100061011     d Tnta25tpd     e ds                  inz
016200061011     d Tnta25tpt     e ds                  inz
016300080609
016400080609     d dlav          e ds
016500080609     d tntbeds       e ds                  extname(tntbe00f)
016600120320
016700120320       // -?ds dati passati da TNSB48R - Manca Tariffa
016800120320     d TNSB48ds      e ds
016900120320     d Parm48          s                   like(tnsb48ds)
017000120713
017100120713       // -?Passaggio dati a TRTB09R - Ricerca codici tassazione
017200120713     d TRTB09DS      e ds
017300140115
017301140219       // -?Passaggio dati a TRULPMGR - Controllo Prezzo base di riferimento
017302140219     d TRULPMGDS     e ds
017303140219
017304140220       // -?Interrogazione e recupero Prezzi medi del Gasolio
017305140220     d TNTA39DS      e ds
017306140220
017400140115       // -?DS campo TPTflo
017500140115     d dTPT01        e ds
017600140115
017700140115       // -?Costante per controllo campi numerici
017800140115     d c_Digits        c                   const('0123456789')
017900090916
018000090916     itivis000
018100090916     i              visesi                      wisesi
018200090916
018300920529     C*****************************************************************
018400920529     C* RIEPILOGO INDICATORI
018500920529     C*****************************************************************
018600920609     C* 01    - IMMISSIONE
018700920609     C* 02    - VARIAZIONE
018800920618     C* 03    - USATO IN VARIAZIONE PER POSIZIONARMI SU CODICE TASSAZ.
018900920618     C* 04    - USATO IN VARIAZIONE PER POSIZIONARMI SU IMPORTO TARIFFA
019000920609     C* 05    - USATO IN VARIAZIONE PER PROTEGGERE I CAMPI
019100920609     C* 06    - TIPO CONSEGNA = ISOLE MINORI
019200920619     C* 07    - PREMUTO CMD6-AGGIORNAMENTO
019300920624     C* 08    - ROLLUP
019400920618     C* 09    - PREMUTO CMD16-ANNULLAMENTO
019500920707     C* 10    - USATO IN INTERROGAZIONE PER PROTEGGERE I CAMPI
019600941111     C* 11    - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
019700920619     C* 12    - PROVENGO DA OFFERTA
019800950119     C* 13    - INDICATORE DI EMISSIONE CAMPO MESSAGGIO DI ERRORE
019900941110     C* 14    - INDICA TIPO TARIFFA/TIPO PAGAMENTO A VALORE
020000920616     C* 15/19 - TASTI DI DUPLICA
020100920707     C* 20    - LOKUP
020200981016     C* 21    - ON  PER IL TIPO CONSEGNA  RCV
020300981016     C*         PROTEGGO IL CAMPO TIPO VALORE.DEVE ESSERE SEMPRE A KG
020400920625     C* 22    - USATO IN ASSICURAZIONE X CONTO PER PROTEGGERE CAMPI
020500920625     C*         DEL FORMATO
020600941111     C* 23    - TASTO DI DUPLICA
020700990217     C* 24    - TARIFFE PARTIC. ASSICURAZIONE X CONTO
020800990217     C* 25    - TAIRFFE PARTIC. RCV
020900920827     C* 26    - USATO X CONDIZIONARE L'EMISSIONE DEGLI ARROTONDAMENTI
021000941110     C* 27    - USATO X CONDIZIONARE L'EMISSIONE DEL   RAPPORTO P/VOL.
021100941111     C* 28    - VISUALIZZO CODICE APPLICAZIONE
021200921007     C* 29    - USATO PER AZZERARE I CAMPI DEL SFLCONTROL NEL CASO IN
021300941110     C*         CUI VENGA VARIATO IL TIPO TARIFFA/TIPO PAGAMENTO
021400920609     C* 30/34 - DI COMODO
021500920609     C* 35/37 - PULIZIA SUBFILE
021600920609     C* 36    - SFLNXTCHG
021700941116     C* 38    - VISUALIZZO IL MINIMO  TARIFFA
021800941116     C* 39    - VISUALIZZO IL MASSIMO TARIFFA
021900941111     C* 40/42 - ERRORE
022000980514     C* 43    - VISUALIZZO IL FLAG MANDATO ASSICURATIVO FITTIZIO
022100990217     C*         SOLO EPR "C" -ASS/C
022200000918     C* 44    - SI TRATTA DI TARUFFA DI CARTELLO PROTEGGO LA COPIA TARIFFE DI
022300000918     C*         CARTELLO
022400941111     C* 45/51 - ERRORE
022500980429     C* 52/59 - ERRORE
022600921126     C* 60    - ON  SIGNIFICA CHE DEVO REGISTRARE IL RECORD
022700941111     C* 61    - USATO PER EMETTERE IL MESSAGGIO DEL CMD14 DI FORZATURA
022800031003     C* 62    - ON SIGNIFICA CHE NON C'E' DETTAGLIO TARIFFE PARTICOLARI
022900990622     C* 63/65 - ERRORE
023000990622     C* 66/73 - ERRORE
023100950119     C* 74    - POSIZIONAMENTO CURSORE SUL CODICE TASSAZIONE PER
023200131217      * 75    - Errore data prezzo base FUEL
023300950119     C*         EMISSIONE CAMPO MESSAGGIO
023400060303     C* 76/77 - ERRORE VI2CTS
023500060303      * 78    - VARIA "d" AC BASE
023600060306      * 79    - Posizionamento cursore
023700000829     C* 80/81 - PULIZIA SUBFILE
023800140219      * 82/85 - Tariffa Particolare "f" Supplemento carburante
023801140219      * 86    - Posizione Cursore Tariffa Particolare "f" Suppl.carbur.
023900140131      * 88    - Tariffa particolare "f" deve avere scaglione >= 999
024000090916      * 89    - Trattativa
024001140219      * 90    - Utente EDPGA
024100120320      * 97    - Pgm richiamato x manca tariffa TNSB48
024200920611     C* 98    - DI COMODO
024300920609     C* 99    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
024400920529     C*****************************************************************
024500920529     C     *ENTRY        PLIST
024600920529     C                   PARM                    KPJBA
024700030715     C                   PARM                    manftc
024800120320     c                   parm                    Parm48
024900920615     C                   MOVEL     KPJBU         PARAM5
024901140219     c                   eval      *in90 = (Knmus = 'EDPGA')
024902140219     c
025000080609
025100080609     c/exec sql
025200080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
025300080609     c/end-exec
025400061011
025500061011     c     *dtaara       define    §azute        azuteds
025600061011     c     *dtaara       define    §datiute      ddatiute
025700061011     c                   in(E)     *dtaara
025800061011     c                   If        %error  or RSUT = *blanks
025900061011     c                   Clear                   Tibs34ds
026000061011     c                   Call      'TIBS34R'
026100061011     c                   Parm                    Tibs34ds
026200061011     c                   In        *dtaara
026300061011     c                   EndIf
026400120320
026500120320
026600120320      /free
026700120320       //?controllo se passati tutti i parametri
026800120320         IF  %parms < 3;
026900120320           clear tnsb48ds;
027000120320           *in97 = *off;
027100120320         ELSE;
027200120320           clear tnsb48ds;
027300120320           tnsb48ds = parm48;
027400120320           *in97 = *on;
027500120320         ENDIF;
027600120320
027700120320         clear OSB48err;
027800120320         clear OSB48msg;
027900120320      /end-free
028000120320
028100120320
028200990615     C* PRENDO LA DATA DEL GIORNO DA TIME
028300990615     C                   TIME                    W0140            14 0
028400990615     C                   MOVE      W0140         UDATE8            8 0
028500990615     C*
028600990615     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
028700990615     C*
028800060802     C                   Z-ADD     UDATE8        G08DAT
028900060802     C                   MOVEL     *BLANK        G08ERR
029000990615     C                   CALL      'XSRDA8'
029100990615     C                   PARM                    WLBDAT
029200990615     C* UDATE A 8 IN AAAA/MM/GG
029300060802     C                   Z-ADD     G08INV        DATEU8            8 0
029400060801     C     *iso          movel     dateu8        Wdataoggi
029500060801      * calcolo la data massima di controllo per supplemento carburante
029600060803     c     Wdataoggi     adddur    06:*m         Wdataeur
029700060802     c     *iso          movel     Wdataeur      Datamax
029800090916
029900090916      * controllo se richiamato da trattativa
030000090916     c                   if        partrat = 'T'
030100090916     c                   eval      *in89 = *on
030200090916     c                   else
030300090916     c                   eval      *in89 = *off
030400090916     c                   endif
030500920529     C*---------------------------------------------------------------*
030600990614     C* ACCESSO TITPT01L/TITPD01L
030700920612     C     KTPT          KLIST
030800920612     C                   KFLD                    VIDCLI
030900920612     C                   KFLD                    VIDCTR
031000920612     C                   KFLD                    VIDPRG
031100920612     C                   KFLD                    VIDFTC
031200000901     C* ACCESSO A TPT/TPD DA SFL COPIA DA CARTELLO
031300000901     C     KTPT9         KLIST
031400000901     C                   KFLD                    VIDCLI
031500000901     C                   KFLD                    VIDCTR
031600000901     C                   KFLD                    VIDPRG
031700000901     C                   KFLD                    V09FTC
031800990614     C* ACCESSO TITPD01L
031900920612     C     KTPD          KLIST
032000920612     C                   KFLD                    VIDCLI
032100920612     C                   KFLD                    VIDCTR
032200920612     C                   KFLD                    VIDPRG
032300920612     C                   KFLD                    VIDFTC
032400920612     C                   KFLD                    VI2ORP
032500940712     C                   KFLD                    VI2SGL
032600050704      *
032700050704     c     ktpd01        klist
032800050704     c                   kfld                    vidcli
032900050704     c                   kfld                    vidctr
033000050704     c                   kfld                    vidprg
033100050704     c                   kfld                    vidftc
033200050704     c                   kfld                    vi2orp
033300940712     C* ACCESSO TABEL00F
033400920608     C     KTAB          KLIST
033500920608     C                   KFLD                    CODUT
033600940712     C                   KFLD                    COD
033700940712     C                   KFLD                    KEY
033800920608     C     KTAB2         KLIST
033900920608     C                   KFLD                    CODUT
034000920608     C                   KFLD                    COD
034100060306
034200060306      * cnclp00f/tfclp00f
034300060306     c     kclp          klist
034400060306     c                   kfld                    codut
034500060306     c                   kfld                    kcc
034600060306     c                   kfld                    ksc
034700140128
034800140128      * TIACB01L
034900140128     c     kACB          klist
035000140128     c                   kfld                    CLPclv
035100140128     c                   kfld                    dateu8
035200140128
035300940712     C***
035400940712     C** DEFINIZIONE CAMPI
035500940712     C***
035600940712     C     *LIKE         DEFINE    TBLCOD        COD
035700940712     C     *LIKE         DEFINE    TBLKEY        KEY
035800941109     C     *LIKE         DEFINE    §1PCTS        WCTS
035900941123     C     *LIKE         DEFINE    §1PCTS        WCTE
036000941109     C     *LIKE         DEFINE    §1PPTV        WPTV
036100940712     C* CAMPI DI DUPLICA
036200940712     C     *LIKE         DEFINE    TPDCTS        DUPCTS
036300940712     C     *LIKE         DEFINE    TPDSGL        DUPSGL
036400941110     C     *LIKE         DEFINE    TPDITR        DUPITR
036500940712     C     *LIKE         DEFINE    TPDMIN        DUPMIN
036600940712     C     *LIKE         DEFINE    TPDMAX        DUPMAX
036700940712     C     *LIKE         DEFINE    TPDAIN        DUPAIN
036800940712     C*
036900920529     C*---------------------------------------------------------------*
037000930223     C***** REPERISCO IL LIMITE RISARCIBILE STABILITO PER LEGGE  *****
037100990907     C*****   DALLA TABELLA GEN PASSANDOGLI LA MONETA            *****
037200990907      *
037300990907     C                   CLEAR                   DSBS02
037400990907     C                   CLEAR                   DGEI
037500990907     C                   MOVEL     'C'           T02MOD
037600990907     C                   MOVEL     KNSIF         T02SIF
037700990907     C                   MOVEL     'GEI'         T02COD
037800990907     C                   MOVEL     VICDIV        T02KE1
037900990907      *
038000990907     C                   CALL      'TIBS02R'
038100990907     C                   PARM                    KPJBA
038200990907     C                   PARM                    DSBS02
038300990907      *
038400990907     C     T02ERR        IFEQ      *BLANKS
038500990907     C                   MOVEL     T02UNI        DGEI
038600990907     C                   ENDIF
038700920609     C*
038800980317     C* CARICO TABELLA CODICI NAZIONE
038900980317     C                   EXSR      CARNAZ
039000080609
039100080609      * carico tabella filiali attive al lasciato avviso
039200080609     c                   exsr      carlav
039300920619     C*
039400920619     C* CONTROLLO SE PROVENGO DA OFFERTA
039500920619     C     FLG           IFNE      ' '
039600920619     C                   Z-ADD     VIDCLI        VIDNRV
039700090916     C                   Z-ADD     VIDCLI        VIDNRVT
039800920619     C                   SETON                                        12
039900110103     c**n89              open      tnvis05l
040000090916     c   89              open      tivis05l
040100110103     c**n89vidnrv        chain     tnvis05l
040200090916     c   89vidnrv        chain     tivis05l
040300090916     c                   If        %found
040400051012     c                   movel     viscmm        kscfil
040500051012     c                   else
040600051012     c                   clear                   kscfil
040700051012     c                   endif
040800110103     c**n89              close     tnvis05l
040900090916     c   89              close     tivis05l
041000060306     c                   open      tfclp00f
041100111122     c                   open      cnclp00f
041200061011
041300061011     c                   open      tiopt01l
041400061011     c                   open      tiopd01l
041500061011
041600051012     c                   else
041700051012      * no offerta
041800051012     c                   movel     vidcli        kscfil
041900060306     c                   open      cnclp00f
042000061011     c                   open      titpt01l
042100061011     c                   open      titpd01l
042200061011     c                   setoff                                       12
042300920619     C                   END
042400990217     C**
042500040429      * verifico codice cliente se inizia con 88888 si tratta di CARTELLO
042600040429     c                   movel     vidcli        w0050             5 0
042700040429     c                   if        w0050 = 88888
042800040429     c                   seton                                        44
042900040429     c                   endif
043000060306      * se non è tariffa di cartello recupero il cod.importanza del CLIENTE
043100060306      * o della VISITA
043200060306     c                   if        not *in44
043300060306     c                   move      vidcli        ksc
043400060306     c  n12kclp          chain     cnclp00f
043500060306     c   12kclp          chain     tfclp00f
043600111122     c                   If        *in12 and not %found(tfclp00f)
043700111122      * se non trovata l'anagrafica provvisoria aggancio l'anagrafica del client
043800111122      * e della trattativa
043900111122     c                   move      visksc        Ksc
044000111122     c     kclp          chain     cnclp00f
044100111122     c                   endif
044200111121     c                   If        clpclv = ' '
044300111121     c                   eval      clpclv = 'C'
044400111121     c                   Endif
044500060306     c                   endif
044600130125
044700130125       //?Imposto flag x bloccare le tariffe particolari in manutenzione
044800130125       //?ma solo TARIFFE e NO CARTELLO
044900130125     c                   eval      dUTE01 = UTEfaf
045000130125     c                   IF        not *in12 and not *in44
045100130125     c                             and §UTEmodtar = *blanks
045200130125     c                   eval      wBlocca = *on
045300130125     c                   ENDIF
045400000829     C*
045500000829     C     CARTAG        TAG
045600921126     C*
045700920529     C* CARICO SUBFILE
045800920529     C                   EXSR      CARICA
045900920609     C* SE NON CARICA NIENTE ESCO
046000920529     C     NRR           IFEQ      0
046100920529     C                   GOTO      FINE
046200920529     C                   END
046300920529     C*
046400920529     C     INIZIO        TAG
046500940913     C                   WRITE     TA67T01
046600940913     C                   WRITE     TA67Z03
046700940913     C                   EXFMT     TA67C02
046800050531     c
046900050531     c                   setoff                                       4013
047000920529     C*
047100921117     C** CMD3  - FINE LAVORO
047200921117     C   KC              MOVEL     'A'           FLG2
047300921118     C   KC
047400921117     C** CMD12 - RITORNO
047500921117     COR KL              GOTO      FINE
047600030718     C** CMD07 - INTERROGAZIONE STORICO VARIAZIONI
047700030718     c                   if        *inkg
047800030718     c                   clear                   tnta04ds
047900030718     c                   eval      i04ksc = vidcli
048000030718     c                   eval      i04ctr = vidctr
048100030718     c                   eval      i04prg = vidprg
048200030718     c                   eval      i04rag = ragdes
048300030718     c                   eval      i04dct = viddcf
048400030718     c                   eval      i04dcv = viddcv
048500030718     c                   eval      i04div = vicdiv
048600030718     c                   eval      i04ndf = flgnet
048700030718      *
048800030718     c                   call      'TNTA04R'
048900030718     c                   parm                    tnta04ds
049000030718      *
049100030718     c                   goto      inizio
049200030718      *
049300030718     c                   endif
049400120320       //?F1 - dati manca tariffa
049500120320     c                   IF        *inka
049600120320     c                   exsr      gesw01
049700120320     c                   goto      inizio
049800120320     c                   ENDIF
049900030718     C*
050000000829     C** CMD13 - COPIA TARIFFE PARTICOLARI DA CARTELLO
050100000829     C     *INKM         IFEQ      *ON
050200000829     C                   EXSR      COPCAR
050300000829     C                   GOTO      CARTAG
050400000829     C                   ENDIF
050500000829     C**
050600920529     C* CONTROLLO DATI SUBFILE
050700920529     C                   SETOFF                                       31
050800940913     C                   READC     TA67S02                                31
050900921118     C   31              GOTO      FINE
051000050531    1C     *IN31         DOWEQ     '0'
051100980317     C*
051200980317     C                   SETOFF                                       0709
051300050531     c                   z-add     1             rec
051400920708     C*
051500050531    2C     VIDSCE        IFEQ      '1'
051600050531     c                   clear                   wfqt
051700050531     c* se tariffa non essitente, non la faccio inserire se in scadenza,
051800050531     c*  da eliminare
051900050531    3c                   if        vidflg=' ' and viheli>*zeros and
052000050531     c                             pardst>=viheli
052100050531     c                   eval      vidmsg=msg(5)
052200050531     c                   eval      %subst(vidmsg:42:2)=vidftc
052300050531     c                   movel     viheli        wdata8            8 0
052400050531     c     *iso          move      wdata8        wdatadmy
052500050531     c                   move      wdatadmy      wdata6            6 0
052600050531     c                   eval      %subst(vidmsg:68:8) =(%editw(wdata6:'  /  / -
052700050531     c                              '))
052800050531     c                   seton                                        13
052900050531    3c                   endif
053000060306
053100060306      * se immissione della tariffa particolare "d " AC BASE la tariffa non deve
053200060306      * avere il flag di rinuncia AC BASE
053300060306     c                   if        not *in13 and vidflg = *blanks and
053400060306     c                             vidftc = 'd ' and parfmp = 'S'
053500060306     c                   eval      vidmsg = msg(8)
053600060306     c                   eval      *in13 = *on
053700060306     c                   endif
053800060306
053900060306      * se immissione della tariffa particolare "d " AC BASE la tariffa non deve
054000060306      * avere una tariffa particolare AC PLUS reale con applicazione completa
054100060306     c                   if        not *in13 and vidflg = *blanks and
054200060306     c                             vidftc = 'd '
054300060306     c                   movel     'C '          v09ftc
054400061011     c  n12ktpt9         chain(n)  titpt01l
054500061011     c   12ktpt9         chain(n)  tiopt01l
054600061011     c                   if        %found and tptatb = *blanks and
054700060306     c                             tpttma = *blanks and tptapl = *blanks
054800060306     c                   eval      vidmsg = msg(9)
054900060306     c                   eval      *in13 = *on
055000060306     c                   endif
055100060306     c                   endif
055200130104
055300130104      * se scelta della tariffa particolare "f " Fuel Surcharge errore
055400130104      * in caso di manutenzione e tariffa, la cartello è libera
055500130128     c*******            IF        not *in13 and vidflg <> *blanks and
055600130128     c*******                      not *in12 and not *in44 and vidftc = 'f '
055700130128     c*******            eval      vidmsg = 'Impossibile manutenzionare la -
055800130128     c*******                      Tariffa Particolare Fuel Surcharge'
055900130128     c*******            eval      *in13 = *on
056000130128     c*******            ENDIF
056100970401     C**
056200050531    3c                   if        not *in13
056300920529     C                   MOVEL     VIDFTC        VI2FTC
056400920612     C                   MOVEL     VIDDES        VI2DTC
056500980317     C* PULIZIA DELLE SCHIERE CODICI TARIFFE PER LA SINGOLA TARIFFA PARTICOLARE
056600980317     C                   MOVEL     *BLANKS       TTR
056700980317     C                   MOVEL     *BLANKS       DTR
056800980317     C                   MOVEL     *BLANKS       TTP
056900980317     C                   MOVEL     *BLANKS       TSP
057000980317     C                   MOVEL     *BLANKS       WTT
057100980429     C                   MOVEL     *BLANKS       ARR
057200980429     C                   MOVEL     *BLANKS       RPV
057300920831     C*
057400941020     C* CARICO I TIPI TARIFFA VALIDI PER LA CONS.PARTICOL. SELEZIONATA
057500941020     C                   MOVEL     '1P'          COD
057600941020     C                   MOVEL     *BLANKS       KEY
057700941020     C                   MOVEL     VIDFTC        KEY
057800941020     C     KTAB          CHAIN     TABEL                              34
057900050531    4C     *IN34         IFEQ      *OFF
058000941109     C* UTT --> TIPI TARIFFA VALIDI PER LA CONSEGNA SELEZIONATA        A
058100980428     C                   MOVEL     TBLUNI        DS1P
058200130125
058300130125       //?Se non posso manutenzionare le tariffe ed è una tariffa
058400130125       //?particolare NON sbloccata errore
058500130125     c                   IF        wBlocca and §1Pmodtp = *blanks
058600130125     c                   eval      vidmsg = 'Impossibile gestire la Tariffa -
058700130125     c                             Particolare selezionata'
058800130125     c                   eval      *in13 = *on
058900130125     c                   goto      nomantp
059000130125     c                   ENDIF
059100130125
059200980428     C                   MOVEL     §1PTT0        WTT(1)
059300980428     C                   MOVEL     §1PTT1        WTT(2)
059400980428     C                   MOVEL     §1PTT2        WTT(3)
059500980428     C                   MOVEL     §1PTT3        WTT(4)
059600980428     C                   MOVEL     §1PTT4        WTT(5)
059700980428     C                   MOVEL     §1PTT5        WTT(6)
059800980428     C                   MOVEL     §1PTT6        WTT(7)
059900980428     C                   MOVEL     §1PTT7        WTT(8)
060000980428     C                   MOVEL     §1PTT8        WTT(9)
060100980428     C                   MOVEL     §1PTT9        WTT(10)
060200980428     C                   MOVEL     §1PTTV        WTT(11)
060300980317     C*
060400980317     C* CARICO TABELLA CODICI TARIFFA
060500980317     C                   EXSR      CARTAB
060600941020     C*
060700941109     C* WCTS --> SE PIENO, IL DETTAGLIO DELLA TARIFFA PARTICOLARE DEVE
060800941109     C*          AVERE COME CODICE TASSAZ. IL CODICE CONTENUTO IN WCTS
060900050531    5C     PARFIE        IFEQ      'I'
061000941123     C* SE TARIFFA ITALIA DEVO PRENDERE COD.TASS. ITALIA
061100941109     C                   MOVEL     §1PCTS        WCTS
061200941123     C                   CLEAR                   WCTE
061300941109     C                   ELSE
061400050531    6C     PARFIE        IFEQ      'E'
061500941123     C* SE TARIFFA ESTERO DEVO PRENDERE COD.TASS. ESTERO
061600941109     C                   MOVEL     §1PCTE        WCTS
061700941123     C                   CLEAR                   WCTE
061800941109     C                   ELSE
061900941123     C* SI TRATTA DI TARIFFA DI CARTELLO QUINDI POSSONO ESISTERE
062000941123     C*   ENTRAMBI I CODICI TASSAZIONE ESPRESSI IN TABELLA
062100941123     C                   MOVEL     §1PCTS        WCTS
062200941123     C                   MOVEL     §1PCTE        WCTE
062300050531    6C                   ENDIF
062400050531    5C                   ENDIF
062500941109     C*
062600941109     C* SALVO LA MASSIMA % PER TARIFFA A VALORE
062700941109     C                   MOVEL     §1PPTV        WPTV
062800941111     C* 11 ON  - VISUALIZZO IL VALORE MERCE E IL FLAG VALORE MERCE
062900941111     C     §1PVVM        COMP      'S'                                    11
063000941111     C* 28 ON  - VISUALIZZO IL TIPO APPLICAZIONE
063100941111     C     §1PVCA        COMP      'S'                                    28
063200941116     C* 38 ON  - VISUALIZZO IL MINIMO  TARIFFA
063300941116     C     §1PVMN        COMP      'S'                                    38
063400941116     C* 39 ON  - VISUALIZZO IL MASSIMO TARIFFA
063500941116     C     §1PVMX        COMP      'S'                                    39
063600980514     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO FITTIZIO
063700050531     C     VIDFTC        COMP      'C'                                    43
063800060802     C* 82 ON  - VISUALIZZO DATA riferimento prezzo base carburante
063900060802     C     §1pdrf        comp      'S'                                    82
064000060802      * Salvo il flag che mi permette di inserire una tariffa particolare
064100021209      * a qtà anche se la tariffa non è a qtà
064200021209     c                   Movel     §1pFqt        Wfqt
064300050531    4C                   ENDIF
064400941020     C*
064500941110     C                   EXSR      VIDEO2
064600050531    3c                   endif
064700130125
064800130125     c     nomantp       tag
064900920827     C*
065000940913     C     NRR           CHAIN     TA67S02                            31
065100920529     C                   MOVEL     ' '           VIDSCE
065200920708     C*
065300920619     C* 07 ON - PREMUTO  CMD6-AGGIORNAMENTO
065400920619     C   07              MOVEL     'S'           VIDFLG
065500990907     C   07              Z-ADD     UDATE8        VIDDUV
065600920708     C*
065700920618     C* 09 ON - PREMUTO  CMD16-ANNULLAMENTO
065800920708     C   09              MOVEL     ' '           VIDFLG
065900990907     C   09              CLEAR                   VIDDUV
066000050531     c* Se errore, mi posizione sulla tariffa particolare
066100050531     c                   if        *in13
066200050531     c                   seton                                        40
066300050531     c                   z-add     nrr           rec
066400050531     c                   endif
066500050531     c
066600940913     C                   UPDATE    TA67S02
066700050531    2C                   END
066800920708     C*
066900050531     c                   if        not *in13
067000940913     C                   READC     TA67S02                                31
067100050531     c                   else
067200050531     c                   seton                                        31
067300050531     c                   endif
067400050531     c
067500050531    1c                   ENDdo
067600921119     C                   GOTO      INIZIO
067700920529     C*
067800920529     C     FINE          TAG
067900921126     C                   MOVEL     PARAM5        KPJBU
068000120320
068100120320     c                   IF        *in97
068200120320     c                   eval      parm48 = tnsb48ds
068300120320     c                   ENDIF
068400120320
068500990917      * CHIUSURA PGM RICERCA BOLLE
068600990917     C                   CLEAR                   DSBS02
068700990917     C                   MOVEL     'C'           T02TLA
068800990917     C                   CALL      'TIBS02R'
068900990917     C                   PARM                    KPJBA
069000990917     C                   PARM                    DSBS02
069100921126     C*
069200920529     C                   SETON                                        LR
069300920609     C*
069400941110     C*--- CARICAMENTO TABELLE ---------------------------------------*
069500920609     C     CARTAB        BEGSR
069600941110     C****  CODICI TARIFFA E DESCRIZIONI  ****
069700920609     C                   MOVEL     'TR'          COD
069800920609     C                   Z-ADD     1             X                 3 0
069900980317     C* CARICO I TIPI TARIFFE CHE VENGONO GESTITE DALLA SINGOLA TARIFFA
070000980317     C* PARTICOLARE
070100990614    1C     WTT(X)        DOWNE     '  '
070200980317     C                   MOVEL     WTT(X)        KEY
070300980317     C     KTAB          CHAIN     TABEL                              30
070400980317    2C     *IN30         IFEQ      '0'
070500980317     C     TBLFLG        ANDEQ     *BLANKS
070600940921     C                   MOVEL     TBLUNI        DSTR
070700940913     C*
070800980317     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE PARTICOLARI
070900980317    3C     §TRUPA        IFEQ      'S'
071000920612     C                   MOVEL     TBLKEY        TTR(X)
071100941110     C                   MOVEL     §TRDES        DTR(X)
071200950112     C* IMPORTO TARIFFA  IN % O £ ("S"=TARIFFA IN % - " "=TARIFFA IN £)
071300941110     C                   MOVEL     §TRTAP        TTP(X)
071400950119     C* SCAGLIONE CON DECIMALI O SENZA ("S"=CON DECIM." "=SENZA DECIM.)
071500950112     C                   MOVEL     §TRSAP        TSP(X)
071600980429     C* ARROTONDAMENTI
071700980429     C                   MOVEL     §TRARR        ARR(X)
071800980429     C* RAPPORTO PESO VOLUME
071900980429     C                   MOVEL     §TRRPV        RPV(X)
072000980317    3C                   END
072100980316     C*
072200980317    2C                   END
072300980317     C*
072400980317     C                   ADD       1             X
072500980317    1C                   ENDDO
072600980317     C*
072700980317     C                   ENDSR
072800980317     C*-------------------------------------------------------------------------
072900980317     C     CARNAZ        BEGSR
073000950112     C****  CODICI NAZIONE  ****
073100950112     C                   Z-ADD     1             X
073200950112     C                   MOVEL     '15'          COD
073300950112     C     KTAB2         SETLL     TABEL
073400950112     C     KTAB2         READE     TABEL                                  32
073500950112     C*
073600950112     C     *IN32         DOWEQ     *OFF
073700950112     C     TBLFLG        IFEQ      ' '
073800950112     C                   MOVEL     TBLUNI        DS15
073900950112     C     §15ITA        IFEQ      'I'
074000950112     C                   MOVEL     TBLKEY        NAZ(X)
074100950112     C                   ADD       1             X
074200950112     C                   ENDIF
074300950112     C                   ENDIF
074400950112     C*
074500950112     C     KTAB2         READE     TABEL                                  32
074600950112     C                   ENDDO
074700050531     c*
074800950112     C*
074900920609     C                   ENDSR
075000080609
075100080609      *------------------------------------------------------------------------*
075200080609      * Carico filiali abilitate al lasciato avviso
075300080609      *------------------------------------------------------------------------*
075400080609     c     carlav        begsr
075500080609
075600080609     c                   clear                   sklav
075700080609     c                   clear                   xx
075800080609
075900080609     c/exec sql
076000080609     c+ declare lav cursor for select tntbe01l.*
076100080609     c+ from tntbe01l
076200080609     c+ where tbecod='LAV' and tbeatb = ''
076300080609     c/end-exec
076400080609
076500080609     c/exec sql
076600080609     c+ open lav
076700080609     c/end-exec
076800080609
076900080609     c                   do        *hival
077000080609
077100080609     c/exec sql
077200080609     c+ fetch next from lav into :tntbeds
077300080609     c/end-exec
077400080609
077500080609     c                   select
077600080609     c                   when      sqlcod = 100
077700080609     c                   leave
077800080609     c                   when      sqlcod < 0
077900080609     c                   other
078000080609     c                   eval      dlav = tbeuni
078100080609      * carico la schiera della filiali abilitate
078200080616     c                   if        d§lavtar > dateu8
078300080609     c                   iter
078400080609     c                   endif
078500080609     c                   eval      xx = xx + 1
078600080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
078700080609
078800080609     c                   endsl
078900080609
079000080609     c                   enddo
079100080609
079200080609     c/exec sql
079300080609     c+ close lav
079400080609     c/end-exec
079500080609
079600080609
079700080609     c                   endsr
079800920609     C*
079900920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
080000920529     C     CARICA        BEGSR
080100920529     C                   SETOFF                                       3031
080200920529     C* PULIZIA SUBFILE
080300920529     C                   EXSR      PULSF
080400920529     C*
080500920529     C                   Z-ADD     0             NRR               4 0
080600920529     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
080700920529     C                   MOVEL     '1P'          COD
080800920608     C     KTAB2         SETLL     TABEL
080900920608     C     KTAB2         READE     TABEL                                  30
081000970325    1C     *IN30         DOWEQ     '0'
081100990621     C* CONTROLLO VALIDITA' TARIFFA PARTICOLARE PER LA GESTIONE TARIFFE
081200970325    2C     TBLFLG        IFEQ      *BLANKS
081300920827     C                   MOVEL     TBLUNI        DS1P
081400020204      * FLAG DI COMODO PER TABELLA 'SP'
081500020204     C                   CLEAR                   FLAGSP
081600080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
081700080609      * per le filiali non abilitate, accetto cartello
081800051012     c                   If        tblkey = 'c       '  and
081900080609     c                             kscfil <> 888
082000080609     c                   if        %lookup(kscfil:sklav) = *zeros
082100080609     c                             and %lookup(999:sklav) = *zeros
082200051012     c                   goto      legtab
082300080609     c                   endif
082400051012     c                   endif
082500060303      * ESCLUDO la tariffa particolare "d" se tariffa di cartello
082600060303     c                   if        tblkey = 'd       '  and *in44
082700060303     c                   goto      legtab
082800060303     c                   endif
082900051012      *
083000020204      * Carico solo le tariffe particolari che hanno flag <> 'N'
083100020204      * Per tariffa con tipo servizio poste controllo flag §1pfpt
083200020201     C     PARTSP        IFEQ      'P'
083300020201     C     §1PFPT        ANDEQ     'N'
083400020201     C                   GOTO      LEGTAB
083500020201     C                   ENDIF
083600020201      * Per tariffa con Network DPD controllo flag §1pfdp
083700020201     C     FLGNET        IFEQ      'D'
083800020201     C     §1PFDP        ANDEQ     'N'
083900020201     C                   GOTO      LEGTAB
084000020201     C                   ENDIF
084100020201      * Per tariffa con Network FedEx controllo flag §1pffe
084200020201     C     FLGNET        IFEQ      'F'
084300020201     C     §1PFFE        ANDEQ     'N'
084400040428      * tariffa FEDEX abilitata solo per le tariffe di cartello e 44 spento
084500040428      * (44 acceso solo se sto modificando una tariffa di cartello)
084600040428     c     flgnet        oreq      'F'
084700040428     c     §1pffe        andeq     'C'
084800040428     c     *in44         andeq     *off
084900020201     C                   GOTO      LEGTAB
085000020201     C                   ENDIF
085100020201      * Per tariffa Italia controllo flag §1ptco
085200020201     C     PARFIE        IFEQ      'I'
085300020205     C     PARTSP        ANDNE     'P'
085400020201     C     §1PTCO        ANDEQ     'N'
085500020201     C                   GOTO      LEGTAB
085600020201     C                   ENDIF
085700020201      * Per tariffa Estero controllo flag §1pfie
085800020201     C     PARFIE        IFEQ      'E'
085900020205     C     FLGNET        ANDEQ     ' '
086000020201     C     §1PFIE        ANDEQ     'N'
086100020201     C                   GOTO      LEGTAB
086200020201     C                   ENDIF
086300020225      * Per tariffe di cartello verifico che non sia tipo servizio "P"
086400020225     C     PARFIE        IFEQ      ' '
086500020225     C     PARTSP        ANDNE     'P'
086600020225     C     §1PTCO        ANDEQ     'N'
086700020225     C                   GOTO      LEGTAB
086800020225     C                   ENDIF
086900020201      *
087000020201      * Utilizzabile in tariffa
087100000420     C**
087200970325    3C     §1PUTA        IFEQ      'S'
087300970325     C*
087400920529     C                   MOVEL     TBLKEY        VIDFTC
087500920529     C                   MOVEL     §1PDES        VIDDES
087600050531     C                   MOVEL     §1Peli        VIheli
087700020201      *
087800020201      * Per tariffa FedEx controllo se devo recupare la descrizione
087900020201      * dalla tabella SP
088000020201     C     FLGNET        IFEQ      'F'
088100020201     C     §1PDFE        ANDEQ     'S'
088200020204     C                   MOVEL     '1'           FLAGSP            1
088300020201     C                   CLEAR                   DSSP
088400020201     C                   MOVEL     'SP'          COD
088500020201     C                   MOVEL     *BLANKS       KEY
088600020201     C                   MOVEL     VIDFTC        KEY
088700020204     C     KTAB          CHAIN     TABEL                              34
088800020201     C     *IN34         IFEQ      *OFF
088900020201     C                   MOVEL     TBLUNI        DSSP
089000020201     C                   ENDIF
089100020201     C     §SPDFE        IFNE      *BLANKS
089200020201     C                   MOVEL     §SPDFE        VIDDES
089300020201     C                   ENDIF
089400020201     C                   ENDIF
089500020201      *
089600920529     C* CAMPO SELEZIONE
089700920529     C                   MOVEL     ' '           VIDSCE
089800920529     C*
089900061011     C  n12KTPT          CHAIN     TITPT000                           31
090000061011     c   12ktpt          chain     tiopt000                           31
090100990903     C                   Z-ADD     0             VIDDUV
090200970325    5C     *IN31         IFEQ      '0'
090300920529     C* CONTROLLO VALIDITA' TARIFFA
090400970325    6C     TPTATB        IFEQ      ' '
090500920529     C* TARIFFA PARTICOLARE GIA' ESISTENTE
090600920529     C                   MOVEL     'S'           VIDFLG
090700990903     C* VALORIZZO L'ULTIMA DATA VARIAZIONE
090800990903     C*
090900990903     C* GIRO DATA
091000990903     C*
091100060802     C                   Z-ADD     TPTDUV        G08INV
091200060802     C                   MOVEL     '3'           G08ERR
091300990903     C                   CALL      'XSRDA8'
091400990903     C                   PARM                    WLBDAT
091500990903     C*
091600060802     C                   Z-ADD     G08DAT        VIDDUV
091700990903     C*
091800920529     C                   ELSE
091900920529     C* TARIFFA ANNULLATA
092000920708     C                   MOVEL     ' '           VIDFLG
092100970325    6C                   END
092200920529     C*
092300970325   X5C                   ELSE
092400920529     C* TARIFFA PARTICOLARE NON ESISTENTE
092500920708     C                   MOVEL     ' '           VIDFLG
092600970325    5C                   END
092700920529     C*
092800990217     C* SE C'E' LA TARIFFA -->VISUALIZZO
092900130607    5C**** VIDFLG        IFEQ      'S'
093000990217     C* SE NON E' UNA TARIFFA CHE RICHIEDE L'ABILITAZIONE --> VISUAL.
093100990217     C*
093200920529     C                   ADD       1             NRR
093300940913     C                   WRITE     TA67S02
093400130607    5C*****              END
093500010920    3C                   END
093600010920    2C                   END
093700000420     C*
093800000420     C     LEGTAB        TAG
093900020204     C     FLAGSP        IFNE      *BLANKS
094000020201     C                   MOVEL     '1P'          COD
094100020204     C     KTAB          SETGT     TABEL
094200020204     C                   ENDIF
094300920611     C     KTAB2         READE     TABEL                                  30
094400970325    1C                   END
094500920529     C                   Z-ADD     1             REC
094600990621     C*
094700920529     C                   ENDSR
094800920529     C*
094900920529     C*--- PULIZIA SUBFILE -------------------------------------------*
095000920529     C     PULSF         BEGSR
095100920529     C                   SETON                                        35
095200940913     C                   WRITE     TA67C02
095300920529     C                   SETOFF                                       35
095400920529     C                   ENDSR
095500000829     C*---------------------------------------------------------------*
095600000829     C*    CARICO DATI IN SUBFILE COPIA TARIFFE DA CARTELLO           *
095700000829     C*---------------------------------------------------------------*
095800000829     C     COPCAR        BEGSR
095900000829     C                   SETOFF                                       8081
096000000829     C* PULIZIA SUBFILE
096100091104     c                   z-add     1             rec
096200000829     C                   SETON                                        80
096300000829     C                   WRITE     TA67C09
096400000829     C                   SETOFF                                       80
096500000829     C*
096600000829     C                   Z-ADD     0             NR9               4 0
096700000829     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
096800000829     C                   MOVEL     '1P'          COD
096900000829     C     KTAB2         SETLL     TABEL
097000000829     C     KTAB2         READE     TABEL                                  30
097100000829    1C     *IN30         DOWEQ     '0'
097200000829     C* CONTROLLO VALIDITA' TARIFFA PARTICOLARE PER LA GESTIONE TARIFFE
097300000829    2C     TBLFLG        IFEQ      *BLANKS
097400000829     C                   MOVEL     TBLUNI        DS1P
097500020204      * FLAG DI COMODO PER TABELLA 'SP'
097600020204     C                   CLEAR                   FLAGSP
097700080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
097800080609      * per le filiali non abilitate, accetto cartello
097900051012     c                   If        tblkey = 'c       '  and
098000080609     c                             kscfil <> 888
098100080609     c                   if        %lookup(kscfil:sklav) = *zeros
098200080609     c                             and %lookup(999:sklav) = *zeros
098300051012     c                   goto      doptab
098400080609     c                   endif
098500051012     c                   endif
098600130125       //?Se non posso manutenzionare le tariffe non carico le tariffe
098700130125       //?particolari NON sbloccate
098800130125     c                   IF        wBlocca and §1Pmodtp = *blanks
098900130125     c                   goto      doptab
099000130125     c                   ENDIF
099100051012      *
099200020204      * Carico solo le tariffe particolari che hanno flag = 'O'
099300020201      * Per tariffa con tipo servizio poste controllo flag §1pfpt
099400020201     C     PARTSP        IFEQ      'P'
099500020201     C     §1PFPT        ANDNE     'O'
099600020201     C                   GOTO      DOPTAB
099700020201     C                   ENDIF
099800020201      * Per tariffa con Network DPD controllo flag §1pfdp
099900020201     C     FLGNET        IFEQ      'D'
100000020201     C     §1PFDP        ANDNE     'O'
100100020201     C                   GOTO      DOPTAB
100200020201     C                   ENDIF
100300020201      * Per tariffa con Network FedEx controllo flag §1pffe
100400020201     C     FLGNET        IFEQ      'F'
100500020201     C     §1PFFE        ANDNE     'O'
100600020201     C                   GOTO      DOPTAB
100700020201     C                   ENDIF
100800020201      * Per tariffa Italia controllo flag §1ptco
100900020201     C     PARFIE        IFEQ      'I'
101000020205     C     PARTSP        ANDNE     'P'
101100020201     C     §1PTCO        ANDNE     'O'
101200020201     C                   GOTO      DOPTAB
101300020201     C                   ENDIF
101400020201      * Per tariffa Estero controllo flag §1pfie
101500020201     C     PARFIE        IFEQ      'E'
101600020205     C     FLGNET        ANDEQ     ' '
101700020201     C     §1PFIE        ANDNE     'O'
101800020201     C                   GOTO      DOPTAB
101900020201     C                   ENDIF
102000020201      *
102100020201      * Utilizzabile in tariffa
102200000829     C**
102300000829    3C     §1PUTA        IFEQ      'S'
102400000829     C*
102500000829     C*
102600000918     C                   MOVEL     TBLKEY        V09FTC
102700061011     C  n12KTPT9         CHAIN     TITPT000                           31
102800061011     c   12ktpt9         chain     tiopt000                           31
102900000829    4C     *IN31         IFEQ      '0'
103000000829     C* CONTROLLO VALIDITA' TARIFFA
103100000829     C     TPTATB        ANDEQ     ' '
103200000829     C* TARIFFA PARTICOLARE GIA' ESISTENTE
103300000829     C                   GOTO      DOPTAB
103400000829     C*
103500000829    4C                   ENDIF
103600000829     C* TARIFFA ANNULLATA   OPPURE
103700000829     C* TARIFFA PARTICOLARE NON ESISTENTE
103800000829     C*                     SCRIVO IL RECORD NEL SUBFILE
103900000829     C                   MOVEL     §1PDES        V09DES
104000020201      *
104100020201      * Per tariffa FedEx controllo se devo recupare la descrizione
104200020201      * dalla tabella SP
104300020201     C     FLGNET        IFEQ      'F'
104400020201     C     §1PDFE        ANDEQ     'S'
104500020204     C                   MOVEL     '1'           FLAGSP            1
104600020201     C                   CLEAR                   DSSP
104700020201     C                   MOVEL     'SP'          COD
104800020201     C                   MOVEL     *BLANKS       KEY
104900020201     C                   MOVEL     V09FTC        KEY
105000020204     C     KTAB          CHAIN     TABEL                              34
105100020201     C     *IN34         IFEQ      *OFF
105200020201     C                   MOVEL     TBLUNI        DSSP
105300020201     C                   ENDIF
105400020201     C     §SPDFE        IFNE      *BLANKS
105500020201     C                   MOVEL     §SPDFE        V09DES
105600020201     C                   ENDIF
105700020201     C                   ENDIF
105800020201      *
105900000829     C* CAMPO SELEZIONE
106000000829     C                   MOVEL     ' '           V09SCE
106100000829     C                   ADD       1             NR9
106200000829     C                   WRITE     TA67S09
106300000829    3C                   END
106400000829    2C                   END
106500000829     C*
106600000829     C     DOPTAB        TAG
106700020204     C     FLAGSP        IFNE      *BLANKS
106800020201     C                   MOVEL     '1P'          COD
106900020204     C     KTAB          SETGT     TABEL
107000020204     C                   ENDIF
107100000829     C     KTAB2         READE     TABEL                                  30
107200000829    1C                   END
107300000829     C*
107400000829     C* GESTIONE DEL SUBFILE DELLA COPIA TARIFFE
107500000915     C                   SETON                                        81
107600000918     C     NR9           COMP      0                                      80
107700000829     C                   Z-ADD     1             NR9
107800000829     C                   WRITE     TA67T01
107900000829     C                   WRITE     TA67Z09
108000000829     C                   EXFMT     TA67C09
108100000829     C*
108200000829    1C     *INKL         IFNE      *ON
108300070521     c     *in80         andeq     *off
108400000829     C* LEGGO IL SUBFILE
108500000901    2C                   DO        *HIVAL
108600000829     C                   READC     TA67S09                                30
108700000915    3C     *IN30         IFEQ      *ON
108800000829     C                   LEAVE
108900000901    3C                   ENDIF
109000000901    3C     V09SCE        IFEQ      '1'
109100061011     C* CARICO LA DS PER PGM DI COPIA TARIFFA
109200061011     c                   clear                   tnta25ds
109300061011     c  n12              eval      ta25tipo = 'T'
109400061011     c   12              eval      ta25tipo = 'O'
109500061011     c                   eval      ta25ksco = vidcli
109600061011     c                   move      vidctr        ta25ctro
109700061011     c                   move      vidprg        ta25prgo
109800061011     c  n12              eval      ta25tipn = 'T'
109900061011     c   12              eval      ta25tipn = 'O'
110000061011     c                   eval      ta25kscn = vidcli
110100061011     c                   move      vidctr        ta25ctrn
110200061011     c                   move      vidprg        ta25prgn
110300061011     c                   eval      ta25tam = 'N'
110400061011     c                   eval      ta25fie = parfie
110500061011     c                   eval      ta25tam = 'N'
110600061011     c                   eval      ta25tad = 'N'
110700061011     c                   eval      ta25tpt = 'N'
110800061011     c                   eval      ta25tpd = 'N'
110900061011     c                   eval      ta25tgc = 'N'
111000061011     c                   eval      ta25cat = 'N'
111100061011     c                   eval      ta25inglo = 'S'
111200061011
111300061011     c                   eval      ta25ftcr = v09ftc
111400061011
111500061011     c                   call      'TNTA25R'
111600061011     c                   parm                    kpjba
111700061011     c                   parm                    tnta25ds
111800061011     c                   parm                    tnta25tad
111900061011     c                   parm                    tnta25tgc
112000061011     c                   parm                    tnta25tpd
112100061011     c                   parm                    tnta25tpt
112200000829     C*
112300000901    3C                   ENDIF
112400000901    2C                   ENDDO
112500000829     C*
112600000901    1C                   ENDIF
112700000829     C*
112800000829     C                   ENDSR
112900920529     C*
113000920529     C*--- SR SECONDA VIDEATA ----------------------------------------*
113100920529     C     VIDEO2        BEGSR
113200920619     C                   SETOFF                                       010207
113300920619     C                   SETOFF                                       090304
113400941116     C                   SETOFF                                       062425
113500060303
113600060303     c                   eval      *in78 = *off
113700060303
113800920529     C*
113900941116     C* 24 ON - TIPO CONSEGNA = "C - ASSICURAZIONE X CONTO"
114000990217     C     §1PASC        IFEQ      'S'
114100990217     C                   SETON                                            24
114200990217     C                   ENDIF
114300941116     C* 25 ON - TIPO CONSEGNA = "R - RCV"
114400990217     C     §1PRCV        IFEQ      'S'
114500990217     C                   SETON                                            25
114600990217     C                   ENDIF
114700941110     C*
114800920609     C* TARIFFA PARTICOLARE NON ESISTENTE
114900920708     C     VIDFLG        IFEQ      ' '
115000920529     C                   SETON                                        01
115100920529     C                   ELSE
115200920609     C* TARIFFA PARTICOLARE GIA' ESISTENTE
115300920529     C                   SETON                                        02
115400920529     C                   END
115500920529     C*
115600921006     C* PULIZIA CAMPI DEL FORMATO E DEL SFLCONTROL
115700921006     C                   MOVEL     ' '           VI2TPG
115800920529     C                   EXSR      PULCTL
115900921126     C*
116000921005     C* CARICO TIPO TARIFFA
116100061011     C   02
116200061011     Cann12KTPT          CHAIN     TITPT000                           33
116300061011     c   02
116400061011     can 12ktpt          chain     tiopt000                           33
116500921005     C   02
116600921005     CANN33              MOVEL     TPTTPG        VI2TPG
116700921005     C   02
116800921005     CANN33              MOVEL     TPTTPG        SAVTPG
116900921118     C   01              MOVEL     ' '           SAVTPG
117000060303
117100060303      * se tariffa particolare "d"
117200060303      * i dati vanno protetti
117300060303     c                   if        vidftc = 'd '
117400060303     c                   eval      *in78 = *on
117500060303      * se non è presente carico i dati di dft
117600060303     c                   if        *in01
117700060303     c                   eval      vi2tpg = '3'
117800060303     c                   endif
117900060303     c                   endif
118000921005     C*
118100921005     C     FOR05         TAG
118200921005     C                   SETOFF                                       22
118300960520     C*
118400940913     C                   WRITE     TA67T01
118500940913     C                   WRITE     TA67Z08
118600921005     C     FOR06         TAG
118700941110     C                   EXFMT     TA67D06
118800921005     C*
118900921005     C** CMD12 - FINE LAVORO
119000921005     C   KL              GOTO      FINE2
119100921005     C*
119200921005     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
119300921005     C   KQ              EXSR      ANNTAR
119400921005     C   KQ              GOTO      FINE2
119500921005     C*
119600921005     C* CONTROLLI FORMATO
119700921005     C                   EXSR      CONTR2
119800921005     C   98              GOTO      FOR05
119900921005     C*
120000921005     C   99              GOTO      FOR06
120100921005     C*
120200921005     C     INVID2        TAG
120300921007     C                   Z-ADD     0             NR2
120400921007     C                   Z-ADD     0             SAVNR2
120500921005     C                   SETON                                        22
120600921005     C* CARICO DATI DEL SFLCONTROL
120700921215     C   02
120800921215     CANN29              MOVEL     TPTDS         COMTPT
120900921006     C   02
121000921006     CANN29              EXSR      CARCTL
121100060303
121200060303      * se tariffa particolare "d" non presente carico i dati di dft
121300060303     c                   if        *in01 and vidftc = 'd '
121400060303      * valore merce 6,2
121500060303     c                   z-add     6,2           vi2vlm
121600060303     c                   z-add     vi2vlm        savvlm
121700060303      * tipo valore '3' già impostato in precedenza se immissione
121800060303     c                   endif
121900060802      * se tariffa particolare "f" non presente carico i dati di dft
122000060802     c                   if        *in01 and vidftc = 'f '
122100060802     c                   seton                                        86
122200061207      *? ATTENZIONE CHIODO!!!  ?
122300061207      * se data del giorno è inferiore a 31/12/2006
122400061207      * chiodo fisso 31/12/2006
122500061207      * altrimenti metto la data del giorno
122600140219     c****               if        dateu8 < 20061231
122700140219     c****               eval      vi2dpb = 31122006
122800140219     c****               else
122900140219     c****               z-add     udate8        vi2dpb
123000140219     c****               endif
123100131216      * verifico se esiste il file del prezzo base da impostare in base alla
123200131216      * data validità
123300131217     c     dateu8        setll     tidpb01l
123400131216     c                   read      tidpb01l
123500131216     c                   if        %found(tidpb01l)
124000140220     c                   z-add     dpbdpb        vo2dpb
124100140115     c                   eval      VI2pma = DPBpma
124200131216     c                   endif
124300131216      * recupero il prezzo del gasolio corrispondente
124400131218     c                   eval      Kpmg = dpbdpb
124500131218     c                   exsr      Rec_pmg
124600131216
124700131218     c                   endif
124800060303      *
124900920611     C* PULIZIA SUBFILE
125000920611     C                   EXSR      PULSF2
125100941110     C*
125200970520     C* 06 ON - APPLICAZIONE CON INOLTRO  PER ISOLE/LOC.DISAGIATE/C.STO
125300970520     C     §1PAIN        COMP      'S'                                    06
125400941110     C*
125500920529     C* CARICO SUBFILE
125600921214     C   02              EXSR      CARSFL
125700920609     C* SCRIVO RECORD DI NUOVA IMMISSIONE
125800921005     C   01              EXSR      SCRIVI
125900920617     C*
126000920617     C                   Z-ADD     1             REC2
126100920611     C     FOR01         TAG
126200940913     C                   WRITE     TA67T01
126300940913     C                   WRITE     TA67Z05
126400941110     C                   WRITE     TA67D06
126500920529     C     FORCT         TAG
126600941110     C                   EXFMT     TA67C07
126700950119     C*
126800950119     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN13)
126900950119     C                   CLEAR                   VI2MSG
127000950119     C                   SETOFF                                       1374
127100140131     c                   setoff                                       62
127200920609     C*
127300111122     C** CMD12 - FINE LAVORO
127400111122     c                   If        *Inkl  and *in62
127500111122     C                   SETON                                        99
127600111122     c                   goto      forct
127700111122     C                   ENDIF
127800111122     c
127900920611     C** CMD12 - FINE LAVORO
128000050704     c                   If        *Inkl
128100111122      /free
128200111122       // Non posso fare F12 se ho annullato tutte le righe di dettaglio
128300111130        IF not *in01;
128400111122         *in62 = *on;
128500111123         nr2 = 1;
128600111123         chain nr2 TA67S07;
128700111123         DOW %found and *in62;
128800111130           IF  VI2cts <> *blanks;
128900111123             *in62 = *off;
129000111123           ENDIF;
129100111123           update TA67S07;
129200111122           nr2 += 1;
129300111122           chain nr2 TA67S07;
129400111122         ENDDO;
129500111123      /end-free
129600111123     c                   IF        *in62
129700111123     c                   eval      *in99 = *in62
129800111123     c                   goto      forct
129900111123     c                   ENDIF
130000111130      /free
130100111130          *in62 = *on;
130200111130          If not *in12 ;
130300111130          setll (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
130400111130          reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
130500111130          DOW  not %eof(TITPD01L);
130600111130            IF  TPDatb = *blanks;
130700111130              *IN62 = *off;
130800111130            ENDIF;
130900111130            reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
131000111130          ENDDO;
131100111130          Endif ;
131200111130          If *in12 ;
131300111130            setll (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
131400111130            reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
131500111130            DOW  not %eof(TIOPD01L);
131600111130              IF  TPDatb = *blanks;
131700111130               *IN62 = *off;
131800111130             ENDIF;
131900111130             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
132000111130           ENDDO;
132100111130           Endif ;
132200111130      /end-free
132300111130     c                   IF        *in62
132400111130     c                   eval      *in99 = *in62
132500111130     c                   goto      forct
132600111130     c                   ENDIF
132700111130     c                   ENDIF
132800111122     c
132900050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
133000050704      * per codice di tassazione
133100050704     c                   Clear                   savcts
133200050704     c                   If        vi2tpg = '2'
133300061011     c  n12ktpt          Setll     titpd01l
133400061011     c   12ktpt          Setll     tiopd01l
133500050704     c                   Do        *Hival
133600061011     c  n12ktpt          Reade(n)  titpd01l
133700061011     c   12ktpt          Reade(n)  tiopd01l
133800061011     c                   If        %eof
133900050704     c                   Leave
134000050704     c                   EndIf
134100050704     c                   If        tpdatb <> *Blanks
134200050704     c                   Iter
134300050704     c                   EndIf
134400050704     c                   If        tpdcts = savcts
134500050704     c                   Eval      *in69 = *on
134600050704     c                   Eval      *in13 = *on
134700050704     c                   Eval      vi2msg = msg(6)
134800050704     c                   Else
134900050704     c                   Eval      savcts = tpdcts
135000050704     c                   EndIf
135100050704     c                   EndDo
135200050704     c                   If        *In13
135300050704     c                   Goto      invid2
135400050704     c                   EndIf
135500050704     c                   EndIf
135600050704     C                   GOTO      FOR05
135700050704     c                   EndIf
135800920611     C*
135900920611     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
136000920611     C   KQ              EXSR      ANNTAR
136100920611     C   KQ              GOTO      FINE2
136101140220     C** CMD04 - Interrogazione prezzi del gasolio
136102140220     C   KD              EXSR      RecPrezzi
136103140220     C   KD              GOTO      Forct
136200920611     C*
136300920611     C** CMD9  - AGGIUNTA RIGHE
136400920611     C   KI
136500920529     C* SCORRIMENTO
136600920611     COR 08              DO
136700920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
136800920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
136900920619     C                   SETON                                        0336
137000920611     C                   EXSR      SCRIVI
137100920529     C                   GOTO      FORCT
137200920529     C                   END
137300920529     C*
137400920611     C* CONTROLLO DATI SUBFILE CONTROL
137500920611     C                   EXSR      CONCTL
137600941111     C   98              GOTO      FOR01
137700941111     C*
137800920619     C   99              GOTO      FORCT
137900920529     C*
138000921007     C* CONTROLLO DATI SUBFILE
138100921007     C                   EXSR      CONSFL
138200921007     C   99              GOTO      FORCT
138201140131
138300921007     C*
138400920623     C** ENTER - REGISTRA
138500920624     C* REGISTRO CAMPI DEL SUBFILE
138600920709     C                   EXSR      REGIS
138700920624     C* REGISTRO CAMPI DEL SUBFILE CONTROL
138800920709     C                   EXSR      REGIS2
138900920623     C*
139000921007     C* SPENGO L' *IN29  PERCHE' IN QUESTO MODO RIESCO A POSIZIONARE
139100921007     C*   IL CURSORE SULLA PRIMA RIGA DI DETTAGLIO  - (ALTRIMENTI
139200921007     C*   RESTAVA POSIZIONATO SUL PRIMO CAMPO DEL SFLCONTROL) -
139300921007     C                   SETOFF                                       29
139500111123      /free
139600111123       // Se ho dato F6 o F14 verifico di avere almeno 1 rcd di dettaglio
139700111123         IF  *inkf or *inkn;
139800111123           *in62 = *on;
139900111130           If not *in12 ;
140000111123           setll (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
140100111123           reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
140200111123           DOW  not %eof(TITPD01L);
140300111123             IF  TPDatb = *blanks;
140400111123               *IN62 = *off;
140500111123             ENDIF;
140600111123             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
140700111123           ENDDO;
140800111130           Endif ;
140900111130           If *in12 ;
141000111130           setll (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
141100111130           reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
141200111130           DOW  not %eof(TIOPD01L);
141300111130             IF  TPDatb = *blanks;
141400111130               *IN62 = *off;
141500111130             ENDIF;
141600111130             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
141700111130           ENDDO;
141800111130           Endif ;
141900111123         ENDIF;
141901140131      /end-free
141902140131     c                   IF        *in62
141903140131     c                   eval      *in99 = *in62
141904140131     c                   goto      invid2
141905140131     c                   ENDIF
142500050704
142600050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
142700050704      * per codice di tassazione
142800050704     c                   Clear                   savcts
142900050704     c                   If        vi2tpg = '2'
143000061011     c  n12ktpt          Setll     titpd01l
143100061011     c   12ktpt          Setll     tiopd01l
143200050704     c                   Do        *Hival
143300061011     c  n12ktpt          Reade(n)  titpd01l
143400061011     c   12ktpt          Reade(n)  tiopd01l
143500061011     c                   If        %eof
143600050704     c                   Leave
143700050704     c                   EndIf
143800050704     c                   If        tpdatb <> *Blanks
143900050704     c                   Iter
144000050704     c                   EndIf
144100050704     c                   If        tpdcts = savcts
144200050704     c                   Eval      *in69 = *on
144300050704     c                   Eval      *in13 = *on
144400050704     c                   Eval      vi2msg = msg(6)
144500050704     c                   Else
144600050704     c                   Eval      savcts = tpdcts
144700050704     c                   EndIf
144800050704     c                   EndDo
144900050704     c                   If        *In13
145000050704     c                   Goto      invid2
145100050704     c                   EndIf
145200050704     c                   EndIf
145300050704
145400921007     C*
145500920623     C** CMD6  - REGISTRA ED ESCE
145600921202     C  NKF
145700921202     CANNKN              GOTO      INVID2
145800920529     C*
145900920708     C     FINE2         ENDSR
146000920609     C*
146100920609     C*--- PULIZIA SUBFILE CONTROL -----------------------------------*
146200920609     C     PULCTL        BEGSR
146300930223     C                   MOVEL     ' '           UNO               1
146400920609     C                   Z-ADD     0             VI2ARL
146500940712     C                   Z-ADD     0             VI2ARF
146600920609     C                   Z-ADD     0             VI2ARO
146700930217     C                   Z-ADD     0             VI2RPV
146800980422     C                   MOVE      ' '           VI2APL
146900980428     C                   MOVE      ' '           VI2MAS
147300140115     c                   clear                   VI2pma
147301140219     C                   CLEAR                   VO2dpb
147302140219     C                   CLEAR                   VO2sca
147303140219     C                   CLEAR                   Vi2pmg
147400941111     C                   CLEAR                   TPG1
147500941111     C                   CLEAR                   DPG1
147600941111     C                   CLEAR                   TPG2
147700941111     C                   CLEAR                   DPG2
147800941111     C                   CLEAR                   TPG3
147900941111     C                   CLEAR                   DPG3
148000941111     C                   CLEAR                   TPG4
148100941111     C                   CLEAR                   DPG4
148200941111     C                   CLEAR                   TPG5
148300941111     C                   CLEAR                   DPG5
148400941111     C                   CLEAR                   TPG6
148500941111     C                   CLEAR                   DPG6
148600980508     C                   CLEAR                   TPG7
148700980508     C                   CLEAR                   DPG7
148800980508     C                   CLEAR                   TPG8
148900980508     C                   CLEAR                   DPG8
149000941111     C*
149100941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
149200941111    1C     *IN11         IFEQ      '1'
149300930217     C   24              Z-ADD     0             VI2VLM
149400930217     C*
149500930217     C* IN IMMISSIONE PER IL TIPO CONSEGNA "R", NEL CAMPO LIMITE
149600941111     C*    RISARCIBILE PROPONGO 12000 PRENDENDOLO DALLA TABELLA "1G"
149700990907     C   25              Z-ADD     §GELRP        VI2VLM
149800930217     C*
149900930223     C* IN IMMISSIONE PROPONGO IL CAMPO TIPO VALORE = 3-PESO KG.
150000930223     C                   MOVEL     '3'           VI2FVM
150100941111     C* DECODIFICA TIPO VALORE MERCE
150200930223     C                   MOVEL     *BLANKS       DESFVM
150300980316     C* DECODIFICA DALLA TABELLA TR
150400980316     C                   MOVEL     'TR'          COD
150500930223     C                   MOVEL     *BLANKS       KEY
150600930223     C                   MOVEL     VI2FVM        KEY
150700930223     C     KTAB          CHAIN     TABEL                              34
150800941111    2C     *IN34         IFEQ      '0'
150900980316     C                   MOVEL     TBLUNI        DSTR
151000980316     C* DECODIFICA DALLA TABELLA TR
151100980316     C                   MOVEL     §TRDES        DESFVM
151200941111    2C                   END
151300941111    1C                   END
151400941110     C*
151500980317     C* CARICO I TIPI TARIFFA
151600941110     C     TTR(1)        IFNE      ' '
151700941110     C                   MOVEL     TTR(1)        TPG1
151800941110     C                   MOVEL     DTR(1)        DPG1
151900941110     C                   END
152000941110     C     TTR(2)        IFNE      ' '
152100941110     C                   MOVEL     TTR(2)        TPG2
152200941110     C                   MOVEL     DTR(2)        DPG2
152300941110     C                   END
152400941110     C     TTR(3)        IFNE      ' '
152500941110     C                   MOVEL     TTR(3)        TPG3
152600941110     C                   MOVEL     DTR(3)        DPG3
152700941110     C                   END
152800941110     C     TTR(4)        IFNE      ' '
152900941110     C                   MOVEL     TTR(4)        TPG4
153000941110     C                   MOVEL     DTR(4)        DPG4
153100941110     C                   END
153200941110     C     TTR(5)        IFNE      ' '
153300941110     C                   MOVEL     TTR(5)        TPG5
153400941110     C                   MOVEL     DTR(5)        DPG5
153500941110     C                   END
153600941110     C     TTR(6)        IFNE      ' '
153700941110     C                   MOVEL     TTR(6)        TPG6
153800941110     C                   MOVEL     DTR(6)        DPG6
153900941110     C                   END
154000980428     C     TTR(7)        IFNE      ' '
154100980428     C                   MOVEL     TTR(7)        TPG7
154200980428     C                   MOVEL     DTR(7)        DPG7
154300980428     C                   END
154400980428     C     TTR(8)        IFNE      ' '
154500980428     C                   MOVEL     TTR(8)        TPG8
154600980428     C                   MOVEL     DTR(8)        DPG8
154700980428     C                   END
154800941110     C*
154900941111     C                   MOVEL     *BLANKS       COMTPT           38
155000920828     C                   ENDSR
155001140220     C*
155002140220     C*--- Recupero Prezzi del Gasolio -------------------------------*
155003140220     C     RecPrezzi     BEGSR
155004140220
155005140220     c                   clear                   tnta39ds
155006140220     c                   call      'TNTA39R'
155007140220     c                   parm                    kpjba
155008140220     c                   parm                    tnta39ds
155009140220
155010140220     c                   If        ota39f03 <> '1'
155011140220     c                   eval      vi2pmg = ota39pmg
155012140220     c                   eval      vo2dpb = ota39dtd
155013140220     c                   eval      vo2sca = ota39sca
155014140220     c                   Endif
155015140220
155016140220     C                   ENDSR
155100921005     C*
155200941116     C*--- CONTROLLO DATI FORMATO ------------------------------------*
155300921005     C     CONTR2        BEGSR
155400921006     C                   SETOFF                                       262729
155500941110     C                   SETOFF                                       989921
155600060802     C                   SETOFF                                       14
155700950119     C                   CLEAR                   WSCD
155800921005     C*
155900921005     C* TIPO TARIFFA OBBLIGATORIO
156000941116    1C     VI2TPG        IFEQ      '?'
156100941110     C                   MOVEL     'TR'          COD
156200941110     C*
156300921005     C                   MOVE      ' '           VI2TPG
156400001020     C                   MOVE      *BLANKS       KEY
156500921005     C                   MOVEL     *BLANKS       DES              25
156600921005     C                   CALL      'X§TABER'
156700921005     C                   PARM                    CODUT
156800921005     C                   PARM                    COD
156900921005     C                   PARM                    KEY
157000921005     C                   PARM                    DES
157100921005     C                   MOVEL     KEY           VI2TPG
157200941110     C                   SETON                                        98
157300921005     C                   GOTO      END2
157400941116    1C                   END
157500921005     C*
157600941116    1C     VI2TPG        IFEQ      ' '
157700921005     C                   SETON                                        4099
157800941116    1C                   END
157900941116     C*
158000921005     C* CONTROLLO SE ESISTE TIPO TARIFFA
158100941020     C                   Z-ADD     1             X
158200941020     C     VI2TPG        LOOKUP    TTR(X)                                 20
158300941110     C* CONTROLLO SE TARIFFA IN % O IN £
158400941110     C   20TTP(X)        COMP      'S'                                    14
158500950119     C* WSCD = "S" ---> SCAGLIONE CON DECIMALI
158600950119     C   20              MOVEL     TSP(X)        WSCD              1
158700980429     C* ARROTONDAMENTI
158800980429     C   20              MOVEL     ARR(X)        WARR              1
158900980429     C* RAPPORTO PESO VOLUME
159000980429     C   20              MOVEL     RPV(X)        WRPV              1
159100941110     C*
159200941110     C*
159300921005     C  N20              SETON                                        4199
159400921005     C   99              GOTO      END2
159500941020     C*
159600950119     C* I TIPI TARIFFA "4" E "5" SI POSSONO ACCETTARE SOLO SE LA TARIFFA
159700950119     C*   CLIENTE HA COME COD.TARIFFA RISPETTIVAMENTE "4xx" E "5xx"
159800021209      * se in tabella '1P' il flag §1PFQT è a Blank
159900021209     c                   If        Wfqt = *Blanks
160000950119     C                   MOVEL     VIDCTR        WCTR              1
160100950119    1C     VI2TPG        IFEQ      '4'
160200950119     C     WCTR          ANDNE     '4'
160300950119     C                   SETON                                        7099
160400950119     C                   GOTO      END2
160500950119    1C                   ENDIF
160600950119    1C     VI2TPG        IFEQ      '5'
160700950119     C     WCTR          ANDNE     '5'
160800950119     C                   SETON                                        7399
160900950119     C                   GOTO      END2
161000950119    1C                   ENDIF
161100021209     c                   EndIf
161200950119     C*
161300950112     C* IN VARIAZIONE: SE TARIFFA A % NON POSSO VARIARE IL TIPO TARIFFA
161400950119    1C   02SAVTPG        IFNE      VI2TPG
161500950119     C                   CLEAR                   WCOM
161600950119     C                   Z-ADD     1             X
161700950119     C*
161800950119     C     SAVTPG        LOOKUP    TTR(X)                                 20
161900950119     C* CONTROLLO SE TARIFFA IN % O IN £
162000950119     C   20TTP(X)        IFEQ      'S'
162100950119     C                   MOVEL     'P'           WCOM              1
162200950119     C                   ENDIF
162300950119     C*
162400950119    2C     *IN14         IFEQ      *ON
162500950119     C     WCOM          ANDEQ     ' '
162600950119     C     *IN14         OREQ      *OFF
162700950119     C     WCOM          ANDEQ     'P'
162800941116     C                   SETON                                        5199
162900941116     C                   GOTO      END2
163000950112    2C                   ENDIF
163100950112    1C                   ENDIF
163200941110     C*
163300921016     C* OGNI VOLTA CHE CAMBIO IL TIPO TARIFFA DEVO AZZERARE TUTTI
163400921016     C*   I CAMPI DEL SFLCONTROL
163500941116    1C     VI2TPG        IFNE      SAVTPG
163600921006     C                   SETON                                        29
163700921005     C                   EXSR      PULCTL
163800941116    1C                   END
163900921005     C*
164000921005     C* CONDIZIONO L'EMISSIONE DEGLI ARROTOND. E DEL RAPPORTO PESO/VOL.
164100980429     C     WARR          IFEQ      'S'
164200980429     C                   SETON                                        26
164300980429     C                   END
164400980429     C*
164500980429     C     WRPV          IFEQ      'S'
164600980429     C                   SETON                                        27
164700980429     C                   END
164800941110     C*
164900941110     C* 21 ON - SE TIPO TARIFFA = 0  PROTEGGO IL CAMPO TIPO VALORE
165000981027     C   25              SETON                                        21
165100921005     C*
165200921005     C     END2          ENDSR
165300920529     C*
165400920611     C*--- PULIZIA SUBFILE -------------------------------------------*
165500920611     C     PULSF2        BEGSR
165600920611     C                   SETON                                        37
165700941111     C                   WRITE     TA67C07
165800920611     C                   SETOFF                                       37
165900920611     C                   ENDSR
166000920611     C*
166100920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
166200921214     C     CARSFL        BEGSR
166300920609     C                   SETOFF                                       323399
166400050704     c                   clear                   savcts
166500920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
166600920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
166700920619     C                   SETON                                        050436
166800920616     C*
166900920611     C                   MOVEL     'CT'          COD
167000920611     C                   MOVEL     *BLANKS       KEY
167100920529     C* RICERCA
167200061011     C  n12KTPT          SETLL     TITPD000
167300061011     c   12ktpt          setll     tiopd000
167400920608     C                   Z-ADD     0             NR2               4 0
167500061011     C  n12KTPT          READE     TITPD000                               32
167600061011     c   12ktpt          reade     tiopd000                               32
167700920529     C* 32 ON - FINE LETTURA
167800920611     C     *IN32         DOWEQ     '0'
167900020207      *
168000050704     C                   SETOFF                                       767769
168100020207      *
168200920611     C* CONTROLLO VALIDITA' TARIFFA
168300920611     C     TPDATB        IFEQ      ' '
168400920529     C* CODICE TASSAZIONE
168500920529     C                   MOVEL     TPDCTS        VI2CTS
168600920608     C* DECODIFICA CODICE TASSAZIONE
168700920609     C                   MOVEL     VI2CTS        KEY
168800920608     C     KTAB          CHAIN     TABEL                              33
168900920612     C  N33              DO
169000920612     C                   MOVEL     TBLUNI        DSCT
169100920612     C                   MOVEL     §CTDES        VI2DCT
169200920612     C                   END
169300920608     C   33              MOVEL     *BLANKS       VI2DCT
169400020207      *
169500020207      * Se è tariffa FedEx devo copiare solo i codici tassazioni
169600020207      * previsti per FedEx
169700020207     C     FLGNET        IFEQ      'F'
169800020207     C     §CTUTF        ANDNE     'S'
169900020207     C                   SETON                                        76
170000020207     C     VI2MSG        IFEQ      *BLANKS
170100020207     C                   SETON                                        13
170200020207     C                   MOVEA     MSG(2)        VI2MSG
170300020207     C                   ENDIF
170400020207     C                   ENDIF
170500020207      * Se NON è tariffa FedEx devo copiare solo i codici tassazioni
170600020207      * NON previsti per FedEx
170700020207     C     FLGNET        IFNE      'F'
170800020207     C     §CTUTF        ANDEQ     'S'
170900020207     C                   SETON                                        77
171000020207     C     VI2MSG        IFEQ      *BLANKS
171100020207     C                   SETON                                        13
171200020207     C                   MOVEA     MSG(3)        VI2MSG
171300020207     C                   ENDIF
171400020207     C                   ENDIF
171500050704      *
171600050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
171700050704      * per codice di tassazione
171800050704     c                   If        vi2tpg = '2'
171900050704     c                   If        tpdcts = savcts
172000050704     c                   Eval      *In69 = *On
172100050704     c                   If        vi2msg = *Blanks
172200050704     c                   Eval      *In13 = *On
172300050704     c                   Movea     msg(6)        vi2msg
172400050704     c                   EndIf
172500050704     c                   Else
172600050704     c                   Eval      savcts = tpdcts
172700050704     c                   EndIf
172800050704     c                   EndIf
172900020207      *
173000970318     C                   MOVEL     TPDORP        VI2ORP
173100920615     C* SCAGLIONE
173200940712     C                   MOVEL     TPDSGL        VI2SGL
173300920608     C* IMPORTO TARIFFA
173400941110     C                   Z-ADD     TPDITR        VI2ITR
173500941110     C                   Z-ADD     TPDITR        SAVITR
173600960520     C                   Z-ADD     TPDITR        SA2ITR
173700920608     C* MINIMO TARIFFA
173800990614     C                   Z-ADD     TPDMIN        VI2MIN
173900990614     C                   Z-ADD     TPDMIN        SAVMIN
174000940713     C* MASSIMO TARIFFA
174100990614     C                   Z-ADD     TPDMAX        VI2MAX
174200990614     C                   Z-ADD     TPDMAX        SAVMAX
174300920611     C* 06 ON - TIPO CONSEGNA = I  CARICO FLAG TARIFFA PROVINCIA
174400920611     C   06              MOVEL     TPDAIN        VI2AIN
174500930219     C   06              MOVEL     TPDAIN        SAVAIN
174600930219     C  N06              MOVEL     ' '           SAVAIN
174700920610     C*
174800061011     C  n12              Z-ADD     TPDNRR        VI2NRR
174900061011     c   12              z-add     tponrr        vi2nrr
175000920608     C* FLAG ANNULLAMENTO
175100920610     C                   MOVEL     ' '           VI2ATB
175200920610     C*
175300920608     C                   ADD       1             NR2
175400920608     C*
175500941111     C                   WRITE     TA67S07
175600920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
175700920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
175800920709     C                   SETOFF                                       0436
175900920611     C                   END
176000920608     C*
176100061011     C  n12KTPT          READE     TITPD000                               32
176200061011     c   12ktpt          reade     tiopd000                               32
176300920611     C                   END
176400920608     C*
176500920609     C                   Z-ADD     NR2           SAVNR2            4 0
176600920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
176700920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
176800920619     C                   SETOFF                                       0436
176900920608     C*
177000920616     C     NR2           IFGT      0
177100920608     C     NR2           DIV       9             DIVIS             3 0
177200920608     C                   MVR                     RESTO             3 0
177300920611     C     RESTO         IFGT      0
177400920611     C                   EXSR      SCRIVI
177500920611     C                   END
177600921126     C*
177700920616     C                   ELSE
177800920618     C                   SETON                                        03
177900920616     C                   EXSR      SCRIVI
178000920616     C                   END
178100920611     C*
178200920608     C                   ENDSR
178300920608     C*
178400920609     C*--- CARICO DATI SUBFILE CONTROL -------------------------------*
178500920608     C     CARCTL        BEGSR
178600061011     C  n12KTPT          CHAIN     TITPT000                           33
178700061011     c   12ktpt          chain     tiopt000                           33
178800941111    1C     *IN33         IFEQ      '0'
178900140115     c                   eval      dTPT01 = TPTflo
179000920925     C*
179100941111     C* 26 ON  - VISUALIZZO ARROTONDAMENTI
179200941111    2C     *IN26         IFEQ      *ON
179300990614     C                   Z-ADD     TPTARL        VI2ARL
179400990614     C                   Z-ADD     TPTARF        VI2ARF
179500990614     C                   Z-ADD     TPTARO        VI2ARO
179600941111    2C                   END
179700941111     C* 27 ON  - VISUALIZZO RAPPORTO PESO/VOLUME
179800920925     C   27              Z-ADD     TPTRPV        VI2RPV
179900920924     C*
180000941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
180100941111    2C     *IN11         IFEQ      *ON
180200920924     C* VALORE MERCE/LIMITE RISARCIBILE
180300920924     C                   Z-ADD     TPTVLM        VI2VLM
180400921009     C                   Z-ADD     TPTVLM        SAVVLM
180500941111     C* TIPO VALORE MERCE
180600940712     C                   MOVEL     TPTFVM        VI2FVM
180700941111     C* DECODIFICA TIPO VALORE MERCE
180800921007     C                   MOVEL     *BLANKS       DESFVM
180900980316     C                   MOVEL     'TR'          COD
181000921007     C                   MOVEL     *BLANKS       KEY
181100921007     C                   MOVEL     VI2FVM        KEY
181200921007     C     KTAB          CHAIN     TABEL                              34
181300941111    3C     *IN34         IFEQ      '0'
181400980316     C                   MOVEL     TBLUNI        DSTR
181500980316     C                   MOVEL     §TRDES        DESFVM
181600941111    3C                   END
181700941111    2C                   END
181800941111     C*
181900941111     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
182000941111     C   28              MOVEL     TPTAPL        VI2APL
182100990217     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO
182200990903     C   43              MOVEL     TPTTMA        VI2MAS
182300140219      * 82 ON  - Visualizzo il prezzo base di riferimento del carburante
182400140219      *          E IL RELATIVO scaglione e la data non visualizzata
182500060802     c                   If        *in82
183100140219     c                   z-add     tptdpb        vo2dpb
183200140115     c                   IF        %check(c_Digits:%subst(TPTflo:1:
183300140115     c                             %len(§TPTpma))) = *zeros
183400140115     c                   eval      VI2pma = §TPTpma
183500140115     c                   ENDIF
183600060802     c                   endif
183700131216      * recupero il prezzo del gasolio corrispondente
183800131218     c                   eval      Kpmg = TPTdpb
183900131218     c                   exsr      Rec_pmg
184000131218
184100941111    1C                   END
184200060802     C
184300920924     C*
184400920608     C                   ENDSR
184500920609     C*
184600920609     C*--- SR AGGIUNTA RIGHE -----------------------------------------*
184700920609     C     SCRIVI        BEGSR
184800920611     C                   SETOFF                                       05
184900020207     C                   SETOFF                                       7677
185000920611     C                   Z-ADD     0             B                 3 0
185100920611     C     RESTO         IFNE      0
185200920611     C                   Z-ADD     RESTO         B
185300920611     C                   Z-ADD     0             RESTO
185400920611     C                   END
185500920609     C* CAMPI DEL SUBFILE
185600920609     C                   Z-ADD     SAVNR2        NR2
185700060306
185800060306      * se tariffa particolare "d" imposto i dati di dft
185900140128      * + il prezzo recuperato dal file TIACB
186000060306      * e scrivo una riga
186100060306     c                   if        vidftc = 'd ' and *in01
186200060307     c                   eval      *in79 = *on
186300060306     c                   eval      vi2cts = 'IT'
186400060306     c                   eval      vi2sgl = 9999999
186500140128     c******             select
186600060306      * clienti Top o Direzionali
186700140128     c******             when      clpclv = 'T' or clpclv = 'D'
186800111130     c****               eval      vi2itr = 0,002
186900140128     c******             eval      vi2itr = 0,003
187000060306      * clienti A
187100140128     c******             when      clpclv = 'A'
187200111130     c****               eval      vi2itr = 0,003
187300140128     c******             eval      vi2itr = 0,004
187400060306      * clienti C o B
187500140128     c******             when      clpclv = 'C' or clpclv = 'B'
187600111130     c****               eval      vi2itr = 0,005
187700140128     c******             eval      vi2itr = 0,007
187800060314      * non trovo la categoria cliente imposto il massimo
187900140128     c******             other
188000111130     c****               eval      vi2itr = 0,005
188100140128     c******             eval      vi2itr = 0,007
188200140128     c******             endsl
188300140128
188400140128       //?Recupero la tariffa AC BASE dal file TIACB
188500140128     c     kACB          setll     TIACB01L
188600140128     c     CLPclv        reade     TIACB01L
188700140128     c                   if        %found(TIACB01L)
188800140128     c                   eval      VI2itr = ACBitr
188900140128     c                   ENDIF
189000140128
189100060705      * controllo se il cliente è presente sulla tabella della forzatura
189200060705      * AC BASE e recupero la tariffa dalla tabella
189300060705     c                   clear                   dsbs02
189400060705     c                   clear                   dfab
189500060705     c                   eval      t02mod = 'C'
189600060705     c                   eval      t02sif = knsif
189700060705     c                   eval      t02cod = 'FAB'
189800060705     c  n12              movel     vidcli        t02ke1
189900060705     c   12              movel     visksc        t02ke1
190000060705     c                   call      'TIBS02R'
190100060705     c                   parm                    kpjba
190200060705     c                   parm                    dsbs02
190300060705     c                   if        t02err = *blanks
190400060705     c                   eval      dfab = t02uni
190500060705     c                   endif
190600060705     c                   if        d§fabitr > 0
190700060705     c                   eval      vi2itr = d§fabitr
190800060705     c                   endif
190900060705
191000060306     c                   z-add     0,001         vi2min
191100060306     c                   z-add     0             vi2max
191200060306     c                   z-add     vi2itr        savitr
191300060306     c                   z-add     vi2itr        sa2itr
191400060306     c                   z-add     vi2min        savmin
191500060306     c                   z-add     0             savmax
191600060306     c                   z-add     0             vi2nrr
191700060306     c                   add       1             nr2
191800060306     c                   add       1             b
191900060306     c                   write     ta67s07
192000060306     c                   setoff                                       0336
192100060306     c                   endif
192200920609     C*
192300920625     C     B             DOWLT     9
192400920609     C                   MOVEL     *BLANKS       VI2CTS
192500920609     C                   MOVEL     *BLANKS       VI2DCT
192600940712     C                   Z-ADD     0             VI2SGL
192700940712     C                   Z-ADD     0             VI2ITR
192800940712     C                   Z-ADD     0             VI2MIN
192900940713     C                   Z-ADD     0             VI2MAX
193000920611     C                   MOVEL     ' '           VI2ATB
193100920609     C* 06 ON - TIPO CONSEGNA = I
193200920610     C   06              MOVEL     'S'           VI2AIN
193300920612     C                   MOVEL     *BLANKS       VI2ORP
193400940712     C                   Z-ADD     0             SAVITR
193500960520     C                   Z-ADD     0             SA2ITR
193600940712     C                   Z-ADD     0             SAVMIN
193700940713     C                   Z-ADD     0             SAVMAX
193800930219     C   06              MOVEL     'S'           SAVAIN
193900930219     C  N06              MOVEL     ' '           SAVAIN
194000920609     C                   Z-ADD     0             VI2NRR
194100941116     C*
194200920609     C                   ADD       1             NR2
194300920609     C                   ADD       1             B
194400941111     C                   WRITE     TA67S07
194500920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
194600920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
194700920619     C                   SETOFF                                       0336
194800920609     C                   END
194900920609     C*
195000920609     C                   Z-ADD     NR2           SAVNR2
195100920609     C                   Z-ADD     NR2           REC2
195200920609     C                   ENDSR
195300920611     C*
195400920611     C*--- ANNULLAMENTO TOTALE TARIFFA -------------------------------*
195500920611     C     ANNTAR        BEGSR
195600920709     C                   SETOFF                                       32
195700920611     C* ANNULLO DETTAGLIO
195800061011     C  n12KTPT          SETLL     TITPD000
195900061011     c   12ktpt          setll     tiopd000
196000920611     C     *IN32         DOWEQ     '0'
196100061011     C  n12KTPT          READE     TITPD000                               32
196200061011     c   12ktpt          reade     tiopd000                               32
196300940713     C  N32TPDATB        IFEQ      ' '
196400941109     C     *IN12         IFEQ      *ON
196500941109     C* OFFERTA
196600061011     C                   DELETE    tiopd000
196700941109     C                   ELSE
196800941109     C* TARIFFA
196900941109     C                   MOVEL     'A'           TPDATB
197000941109     C                   MOVEL     ' '           TPDFTR
197100040915     C                   Z-ADD     dateu8        TPDDTR
197200990614     C                   UPDATE    TITPD000
197300941109     C                   END
197400921217     C                   END
197500920611     C                   END
197600921130     C*
197700940713     C* ANNULLO TESTATA
197800061011     C  n12KTPT          CHAIN     TITPT000                           31
197900061011     c   12ktpt          chain     tiopt000                           31
198000940713     C     *IN31         IFEQ      *OFF
198100941109     C     *IN12         IFEQ      *ON
198200941109     C* OFFERTA
198300061011     C                   DELETE    tiopt000
198400941109     C                   ELSE
198500941109     C* TARIFFA
198600990615     C* DATA ULTIMA VARIAZIONE
198700990615     C                   Z-ADD     DATEU8        TPTDUV
198800941109     C                   MOVEL     'A'           TPTATB
198900941109     C                   MOVEL     ' '           TPTFTR
199000040915     C                   Z-ADD     dateu8        TPTDTR
199100990614     C                   UPDATE    TITPT000
199200030715      * mi salvo la tariffa particolare annullata
199300030715     c                   movel(p)  TPTftc        w_manftc
199400030715     c                   move      'A'           W_manftc
199500030715     c                   z-add     1             WW
199600030715     c     w_manftc      lookup    manftc                                 87
199700030715     c                   if        not %found
199800030715     c     '   '         lookup    manftc(ww)                             87
199900030715     c                   if        %found
200000030715     c                   movea     w_manftc      manftc(ww)
200100030715     c                   endif
200200030715     c                   endif
200300941109     C                   END
200400940713     C                   END
200500921130     C*
200600921217     C*
200700920618     C                   SETON                                        09
200800920611     C                   ENDSR
200900920609     C*
201000920609     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
201100920609     C     CONSFL        BEGSR
201200920619     C                   SETOFF                                       993304
201300140131     c                   setoff                                       7976
201400920609     C                   Z-ADD     1             NR2
201500031003     C* INDICATORE 62 -> RIMANE ACCESO SE NON CI SONO RECORD VALORIZZATI
201600031003     C*                  NEL SUBFILE
201700031003     C                   SETON                                        62
201701140131
201702140131     c                   If        vidftc = 'f '
201703140131     C* INDICATORE 88 -> RIMANE ACCESO SE NON CI SONO scaglioni > = 9999
201704140131     C*                  NEL SUBFILE  solo per il fuel
201705140131     C                   SETON                                        88
201709140131
201710140131     c                   else
201711140219     c                   setoff                                       88
201712140131     c                   Endif
201800920609     C*
201900941111     C     NR2           CHAIN     TA67S07                            33
202000941111     C*
202100941111    1C     *IN33         DOWEQ     '0'
202200920624     C   15              MOVEL     DUPCTS        VI2CTS
202300940712     C   16              Z-ADD     DUPSGL        VI2SGL
202400940712     C   17              Z-ADD     DUPITR        VI2ITR
202500940712     C   18              Z-ADD     DUPMIN        VI2MIN
202600940713     C   23              Z-ADD     DUPMAX        VI2MAX
202700920624     C   19              MOVEL     DUPAIN        VI2AIN
202800920616     C*
202900941111    2C     VI2CTS        IFNE      *BLANKS
203000940713     C     RECORD        ORNE      *BLANKS
203100940713     C     RECORD        ANDNE     *ZEROS
203200031003      *
203300031003     C                   SETOFF                                       62
203400941111     C*
203500920609     C* CONTROLLO SE RECORD DI NUOVA IMMISSIONE
203600941111    3C     VI2NRR        IFEQ      0
203700920609     C                   SETOFF                                       05
203800920827     C*
203900920827     C*** CODICE TASSAZIONE
204000920609     C* CODICE TASSAZIONE OBBLIGATORIO
204100941111    4C     VI2CTS        IFEQ      *BLANKS
204200920611     C                   SETON                                        4599
204300920611     C                   GOTO      ENDSFL
204400941111    4C                   END
204500010920     C* CODICE TASSAZIONE "EE" ERRATO
204600020328      * se tariffa DPD
204700010920    4C     VI2CTS        IFEQ      'EE'
204800020325     C     FLGNET        ANDEQ     'D'
204900011022     C                   SETON                                        741399
205000011022     C                   MOVEA     MSG(1)        VI2MSG
205100011022     C                   GOTO      ENDSFL
205200010920    4C                   ENDIF
205300020328
205400020328      * se tariffa italia non accetto codice tassazione 'EE'
205500020328     c                   if        parfie = 'I' and vi2cts = 'EE'
205600020328     c                   eval      *in74 = *on
205700020328     c                   eval      *in13 = *on
205800020328     c                   eval      *in99 = *on
205900020328     c                   eval      vi2msg = msg(1)
206000020328     c                   goto      endsfl
206100020328     c                   endif
206200020328      * se tariffa estera non accetto codice tassazione 'IT'
206300020328     c                   if        parfie = 'E' and vi2cts = 'IT'
206400020328     c                   eval      *in74 = *on
206500020328     c                   eval      *in13 = *on
206600020328     c                   eval      *in99 = *on
206700020328     c                   eval      vi2msg = msg(4)
206800020328     c                   goto      endsfl
206900020328     c                   endif
207000020328
207100941020     C* SE LA CONSEGNA PARTICOLARE IN GESTIONE RICHIEDE UN PARTICOLARE
207200941111     C*   CODICE DI TASSAZIONE E' NECESSARIO CHE SIA PRESENTE SOLO
207300941020     C*   QUELLO NELLE RIGHE DI DETTAGLIO
207400950119    4C     WCTS          IFNE      *BLANKS
207500950119     C     WCTE          ANDNE     *BLANKS
207600941123     C*
207700950119    5C     WCTS          IFNE      VI2CTS
207800950119     C     WCTE          ANDNE     VI2CTS
207900950119     C                   MOVEA     WCTS          MS2(49)
208000950119     C                   MOVEA     WCTE          MS2(56)
208100950119     C                   MOVEA     MS2           VI2MSG
208200950119     C                   SETON                                        741399
208300941020     C                   GOTO      ENDSFL
208400941123    5C                   ENDIF
208500941123     C*
208600941123   X4C                   ELSE
208700941123     C*
208800941123    5C     WCTS          IFNE      *BLANKS
208900941123     C     WCTS          ANDNE     VI2CTS
209000950119     C                   MOVEA     WCTS          MS1(50)
209100950119     C                   MOVEA     MS1           VI2MSG
209200950119     C                   SETON                                        741399
209300941123     C                   GOTO      ENDSFL
209400941123    5C                   ENDIF
209500950119     C*
209600950119    5C     WCTE          IFNE      *BLANKS
209700950119     C     WCTE          ANDNE     VI2CTS
209800950119     C                   MOVEA     WCTE          MS1(50)
209900950119     C                   MOVEA     MS1           VI2MSG
210000950119     C                   SETON                                        741399
210100950119     C                   GOTO      ENDSFL
210200950119    5C                   ENDIF
210300941123    4C                   ENDIF
210400120713
210500120713      * ricerca codice tassazione
210600120713     c                   IF        %scan('?':VI2cts) > 0
210700120713     c                   clear                   TRTB09DS
210800120713     c                   eval      TB09fun = '1'
210900120713     c                   eval      TB09ord = '1'
211000120713     c                   IF        flgnet = 'D'
211100120713     c                   eval      TB09est = 'E'
211200120713     c                   ELSE
211300120713     c                   eval      TB09est = parfie
211400120713     c                   ENDIF
211500120713     c                   eval      TB09uta = 'S'
211600120713     c                   IF        flgnet = 'F'
211700120713     c                   eval      TB09utf = 'S'
211800120713     c                   ELSE
211900120713     c                   eval      TB09utf = 'N'
212000120713     c                   ENDIF
212100120713     c                   eval      kpjbu = TRTB09DS
212200120713     c                   call      'TRTB09R'
212300120713     c                   parm                    kpjba
212400120713     c                   eval      TRTB09DS = kpjbu
212500120713     c                   IF        TB09cod <> *blanks
212600120713     c                   eval      VI2cts = TB09cod
212700120713     c                   ELSE
212800120723     c                   clear                   VI2cts
212900120713     c                   ENDIF
213000120713     c                   ENDIF
213100941123     C*
213200920609     C* SE IMMESSO COD.TASSAZIONE CONTROLLO SE ESISTE
213300920609     C                   MOVEL     'CT'          COD
213400920611     C                   MOVEL     *BLANKS       KEY
213500920609     C                   MOVEL     VI2CTS        KEY
213600920609     C     KTAB          CHAIN     TABEL                              34
213700920611     C* CONTROLLO VALIDITA' CODICE TASSAZIONE
213800941111    4C  N34TBLFLG        IFNE      *BLANKS
213900920611     C                   SETON                                        4699
214000920611     C                   GOTO      ENDSFL
214100941111   X4C                   ELSE
214200950112     C                   MOVEL     TBLUNI        DSCT
214300950112     C*
214400950112     C*
214500920611     C* DECODIFICO CODICE TASSAZIONE
214600920617     C                   MOVEL     §CTDES        VI2DCT
214700990614     C                   MOVE      §CTCOR        VI2ORP
214800941111    4C                   END
214900950112     C*
215000920611     C* 34 ON - CODICE TASSAZIONE INESISTENTE
215100920611     C   34              SETON                                        4799
215200920611     C   34              GOTO      ENDSFL
215300050304      * errore bloccante se codice tassazione non utilizzabile in gestione
215400050304      * tariffe clienti
215500050304     c                   If        §ctuta = 'N'
215600050304     c                   Eval      *In47 = *On
215700050304     c                   Eval      *In99 = *On
215800050304     c                   Goto      endsfl
215900050304     c                   EndIf
216000020207      *
216100020207      * Se è tariffa FedEx devo copiare solo i codici tassazioni
216200020207      * previsti per FedEx
216300020207     C     FLGNET        IFEQ      'F'
216400020207     C     §CTUTF        ANDNE     'S'
216500020207     C                   SETON                                        769913
216600020207     C                   MOVEA     MSG(2)        VI2MSG
216700020207     C                   GOTO      ENDSFL
216800020207     C                   ENDIF
216900020207      * Se NON è tariffa FedEx devo copiare solo i codici tassazioni
217000020207      * NON previsti per FedEx
217100020207     C     FLGNET        IFNE      'F'
217200020207     C     §CTUTF        ANDEQ     'S'
217300020207     C                   SETON                                        779913
217400020207     C                   MOVEA     MSG(3)        VI2MSG
217500020207     C                   GOTO      ENDSFL
217600020207     C                   ENDIF
217700070226      * Se tariffa estero accetto solo estero
217800070226     c                   if        parfie = 'E' and §ctest = *Blanks
217900070226     c                   eval      *in13 = *on
218000070226     c                   eval      *in76 = *on
218100070226     c                   eval      *in99 = *on
218200070226     c                   movea     msg(12)       vi2msg
218300070226     c                   goto      endsfl
218400070226     c                   endif
218500070226      * Se tariffa DPD accetto solo estero
218600070226      * tranne il codice generico IT xchè va bene
218700070226     c                   if        flgnet = 'D' and §ctest = *Blanks and
218800070227     c                             vi2cts <> 'IT'
218900070226     c                   eval      *in13 = *on
219000070226     c                   eval      *in76 = *on
219100070226     c                   eval      *in99 = *on
219200070226     c                   movea     msg(12)       vi2msg
219300070226     c                   goto      endsfl
219400070226     c                   endif
219500070226      * Se tariffa italia accetto solo italia
219600070226     c                   if        parfie = 'I' and flgnet <> 'D' and
219700070226     c                                              §ctest <> *Blanks
219800070226     c                   eval      *in13 = *on
219900070226     c                   eval      *in76 = *on
220000070226     c                   eval      *in99 = *on
220100070226     c                   movea     msg(12)       vi2msg
220200070226     c                   goto      endsfl
220300070226     c                   endif
220400050704
220500050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
220600050704      * per codice di tassazione
220700050704     c                   If        vi2tpg = '2' and vi2atb = *Blanks
220800061011     c  n12ktpd01        Setll     titpd01l
220900061011     c   12ktpd01        Setll     tiopd01l
221000050704     c                   Do        *Hival
221100061011     c  n12ktpd01        Reade(n)  titpd01l
221200061011     c   12ktpd01        Reade(n)  tiopd01l
221300061011     c                   If        %eof
221400050704     c                   Leave
221500050704     c                   EndIf
221600050704     c                   If        tpdatb <> *Blanks
221700050704     c                   Iter
221800050704     c                   EndIf
221900050704     c                   Eval      *in74 = *on
222000050704     c                   Eval      *in13 = *on
222100050704     c                   Eval      *in99 = *on
222200050704     c                   Eval      vi2msg = msg(6)
222300050704     c                   Goto      endsfl
222400050704     c                   EndDo
222500050704     c                   EndIf
222600020207      *
222700920827     C*
222800920827     C*** SCAGLIONE OBBLIGATORIO
222900941111    4C     VI2SGL        IFEQ      0
223000920609     C                   SETON                                        4899
223100920609     C                   GOTO      ENDSFL
223200950119   X4C                   ELSE
223300990622     C* SE TIPO TARIFFA/PAGAMENTO E' A VALORE LO SCAGLIONE PUO' ESSERE UN VALORE
223400990622     C* ANCHE CON I DECIMALI IN BASE ALLA DIVISA
223500990622    5C     *IN14         IFEQ      *ON
223600990622     C                   MOVE      VI2SGL        W0030
223700990622    6C     W0030         IFGT      0
223800990622     C     FLGDEC        ANDNE     'S'
223900990622     C                   SETON                                        7199
224000990622     C                   GOTO      ENDSFL
224100990622    6C                   ENDIF
224200990622     C**
224300990622   X5C                   ELSE
224400950119     C* WSCD = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
224500950119     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
224600990622    6C     WSCD          IFEQ      ' '
224700950119     C                   MOVE      VI2SGL        W0030
224800950119     C     W0030         IFGT      0
224900950119     C                   SETON                                        7199
225000950119     C                   GOTO      ENDSFL
225100950119     C                   ENDIF
225200990622    6C                   ENDIF
225300990622    5C                   ENDIF
226300080922    4C                   END
226400920827     C*
226500920827     C*** CONTROLLO SE RIGHE DOPPIE
226600061011     C  n12KTPD          CHAIN     TITPD000                           32
226700061011     c   12ktpd          chain     tiopd000                           32
226800061011     c  n32
226900061011    4CanN12TPDNRR        IFNE      VI2NRR
227000941111     C     TPDATB        ANDEQ     ' '
227100920709     C                   SETON                                        4999
227200920709     C                   GOTO      ENDSFL
227300941111    4C                   END
227400061011     c  n32
227500061011    4can 12tponrr        ifne      VI2NRR
227600061011     C     TPDATB        ANDEQ     ' '
227700061011     C                   SETON                                        4999
227800061011     C                   GOTO      ENDSFL
227900061011    4C                   END
228000920828     C*
228100941111   X3C                   ELSE
228200920617     C                   SETON                                        05
228300941111    3C                   END
228400060307
228500060307      * se tariffa particolare 'd ' AC BASE non posso andare al di sotto del
228600060307      * minimo indicato in DV 1463
228700060705      * o in tabella FAB
228800060313     c                   If        vidftc = 'd '
228900060705      * controllo se il cliente è presente sulla tabella della forzatura
229000060705      * AC BASE e controllo la tariffa inserita con quanto presente in tabella
229100060705     c                   clear                   dsbs02
229200060705     c                   clear                   dfab
229300060705     c                   eval      t02mod = 'C'
229400060705     c                   eval      t02sif = knsif
229500060705     c                   eval      t02cod = 'FAB'
229600060705     c  n12              movel     vidcli        t02ke1
229700060705     c   12              movel     visksc        t02ke1
229800060705     c                   call      'TIBS02R'
229900060705     c                   parm                    kpjba
230000060705     c                   parm                    dsbs02
230100060705     c                   if        t02err = *blanks
230200060705     c                   eval      dfab = t02uni
230300060705     c                   endif
230400060705     c                   if        vi2itr < d§fabitr
230500060705     c                   eval      vi2msg = msg(11)
230600060705     c                   eval      %subst(vi2msg:38:12) = %editc(d§fabitr:'4')
230700060705     c                   eval      *in13 = *on
230800060705     c                   eval      *in79 = *on
230900060705     c                   eval      *in99 = *on
231000060705     c                   goto      endsfl
231100060705     c                   endif
231200060705      * se cliente presente in tabella FAB non faccio gli altri controlli
231300060705     c                   if        d§fabitr = *zeros
231400140128
231500140128       //?Recupero la tariffa AC BASE dal file TIACB
231600140128     c     kACB          setll     TIACB01L
231700140128     c     CLPclv        reade     TIACB01L
231800140128     c                   IF        %found(TIACB01L) and
231900140128     c                             VI2itr < ACBitr
232000140128     c                   eval      vi2msg = msg(7)
232100140128     c                   eval      %subst(vi2msg:10:1) = clpclv
232200140128     c                   eval      VI2msg = %trim(VI2msg) + ' ' +
232300140128     c                             %trim(%editc(ACBitr:'4'))
232400140128     c                   eval      *in13 = *on
232500140128     c                   eval      *in79 = *on
232600140128     c                   eval      *in99 = *on
232700140128     c                   goto      endsfl
232800140128     c                   ENDIF
232900140128
233000140128     c******             Select
233100060307      * clienti Top o Direzionali
233200140128     c******             when      (clpclv = 'T' or clpclv = 'D') and
233300140128     c******                       vi2itr < 0,003
233400111130     c****                         vi2itr < 0,002
233500140128     c******             eval      vi2msg = msg(7)
233600111130     c****               eval      %subst(vi2msg:50:5) = '0,002'
233700140128     c******             eval      %subst(vi2msg:50:5) = '0,003'
233800140128     c******             eval      %subst(vi2msg:10:1) = clpclv
233900140128     c******             eval      *in13 = *on
234000140128     c******             eval      *in79 = *on
234100140128     c******             eval      *in99 = *on
234200140128     c******             goto      endsfl
234300060307      * clienti A
234400140128     c******             when      clpclv = 'A' and
234500140128     c******                       vi2itr < 0,004
234600111130     c****                         vi2itr < 0,003
234700140128     c******             eval      vi2msg = msg(7)
234800111130     c****               eval      %subst(vi2msg:50:5) = '0,003'
234900140128     c******             eval      %subst(vi2msg:50:5) = '0,004'
235000140128     c******             eval      %subst(vi2msg:10:1) = clpclv
235100140128     c******             eval      *in13 = *on
235200140128     c******             eval      *in79 = *on
235300140128     c******             eval      *in99 = *on
235400140128     c******             goto      endsfl
235500060307      * clienti C o B
235600140128     c******             when      (clpclv = 'C' or clpclv = 'B') and
235700140128     c******                       vi2itr < 0,007
235800111130     c****                         vi2itr < 0,005
235900140128     c******             eval      vi2msg = msg(7)
236000111130     c****               eval      %subst(vi2msg:50:5) = '0,005'
236100140128     c******             eval      %subst(vi2msg:50:5) = '0,007'
236200140128     c******             eval      %subst(vi2msg:10:1) = clpclv
236300140128     c******             eval      *in13 = *on
236400140128     c******             eval      *in79 = *on
236500140128     c******             eval      *in99 = *on
236600140128     c******             goto      endsfl
236700140128     c******             endsl
236800060705     c                   endif
236900060313     c                   endif
237000140115
237001140131      //?Se tariffa particolare 'f ' FUEL se importo > 0 o scaglione >=9999
237100140131      //?spendo INDICATORI DI ERRORE
238101140131     c                   IF        VIDftc = 'f ' and
238102140131     c                             VI2sgl >= 9999
238103140220     c                             and  VI2atb = ' '
238104140131     c                   setoff                                       88
238105140131     c                   ENDIF
238200140115
238300920827     C*
238400930219     C*** PER TUTTE CONSEGNE PARTICOLARI SI PUO' ACCETTARE
238500921202     C*     L'IMPORTO TARIFFA = 0  CON IL CMD14 DI FORZATURA
238600941111    3C     VI2ITR        IFEQ      0
238700930219     C  NKN              SETON                                        6199
238800921202     C   99              GOTO      ENDSFL
238900960520     C*
239000941111   X3C                   ELSE
239100960520     C* CONTROLLO CHE NON SUPERI LA MASSIMA % PER LA TARIFFA A VALORE:
239200960520     C*   SE LA SUPERA EMETTO MESSAGGIO FORZABILE
239300941111    4C     *IN14         IFEQ      *ON
239400960520     C*
239500960704    5C     VI2ITR        IFNE      SA2ITR
239600960520     C     VI2ITR        ANDGT     WPTV
239700941110     C                   SETON                                        6699
239800960704     C                   MOVEL     VI2ITR        SA2ITR
239900941110     C                   GOTO      ENDSFL
240000941111    5C                   END
240100941111     C*
240200941111   X4C                   ELSE
240300941110     C* SE SI TRATTA DI TARIFFA IN £ NON SI POSSONO INSERIRE I DECIMALI
240400941111     C                   MOVE      VI2ITR        W0030             3 0
240500941111    5C     W0030         IFGT      0
240600990622     C     FLGDEC        ANDNE     'S'
240700941110     C                   SETON                                        6899
240800941110     C                   GOTO      ENDSFL
240900941111    5C                   END
241000941111    4C                   END
241100941111    3C                   END
241200990622     C*
241300990622     C*** IMPORTO MINIMO  TARIFFA
241400990622     C* CONTROLLO I DECIMALI
241500990622    3C     VI2MIN        IFGT      0
241600990622     C                   MOVE      VI2MIN        W0030             3 0
241700990622    4C     W0030         IFGT      0
241800990622     C     FLGDEC        ANDNE     'S'
241900990622     C                   SETON                                        6499
242000990622     C                   GOTO      ENDSFL
242100990622    4C                   END
242200990622    3C                   END
242300990622     C*
242400990622     C*** IMPORTO MASSIMO TARIFFA
242500990622     C* CONTROLLO I DECIMALI
242600990622    3C     VI2MAX        IFGT      0
242700990622     C                   MOVE      VI2MAX        W0030             3 0
242800990622    4C     W0030         IFGT      0
242900990622     C     FLGDEC        ANDNE     'S'
243000990622     C                   SETON                                        6599
243100990622     C                   GOTO      ENDSFL
243200990622    4C                   END
243300990622    3C                   END
243400920827     C*
243500940713     C*** IMPORTO MASSIMO TARIFFA
243600940713     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
243700941111    3C     VI2MAX        IFGT      0
243800940713     C     VI2MAX        ANDLT     VI2MIN
243900940713     C                   SETON                                        6399
244000940713     C                   GOTO      ENDSFL
244100941111    3C                   END
244200940713     C*
244300920616     C* SE NON CI SONO ERRORI AGGIORNO E VADO AVANTI A LEGGERE
244400920616     C                   MOVEL     VI2CTS        DUPCTS
244500940712     C                   Z-ADD     VI2SGL        DUPSGL
244600941110     C                   Z-ADD     VI2ITR        DUPITR
244700940712     C                   Z-ADD     VI2MIN        DUPMIN
244800940713     C                   Z-ADD     VI2MAX        DUPMAX
244900920616     C   06              MOVEL     VI2AIN        DUPAIN
245000941111     C*
245100941111   X2C                   ELSE
245200930225     C                   SETOFF                                       05
245300941111    2C                   END
245400920616     C*
245500920611     C     ENDSFL        TAG
245600920610     C   99              Z-ADD     NR2           REC2
245700920619     C*
245800941111     C                   UPDATE    TA67S07
245900920619     C   99              SETON                                        33
246000920619     C*
246100921126     C                   ADD       1             NR2
246200941111     C  N99NR2           CHAIN     TA67S07                            33
246300941111    1C                   END
246400031003      * SE E' ACCESO L'INDICATORE 62 SEGNALO ERRORE CHE E' OBBLIGATORIO IL
246500031003      * DETTAGLIO TARIFFA
246600140219     C                   IF        *IN62 or *in88
246700031003     C                   SETON                                        99
246800031003     C                   ENDIF
246900920611     C                   ENDSR
247000920610     C*
247100920611     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
247200920611     C     CONCTL        BEGSR
247300060802     C                   SETOFF                                       999886
247400921126     C* AZZERO CAMPI DI DUP
247500940712     C                   MOVEL     *BLANKS       DUPCTS
247600940712     C                   Z-ADD     0             DUPSGL
247700941110     C                   Z-ADD     0             DUPITR
247800940712     C                   Z-ADD     0             DUPMIN
247900940713     C                   Z-ADD     0             DUPMAX
248000940712     C   06              MOVEL     *BLANKS       DUPAIN
248100060307
248200060307      * faccio subito i controlli per AC BASE e AC PLUS
248300060307
248400060307      * se tariffa particolare "d " AC BASE la tariffa non deve
248500060307      * avere il flag di rinuncia AC BASE
248600060307     c                   if        vidftc = 'd ' and parfmp = 'S'
248700060307     c                   eval      vi2msg = msg(8)
248800060307     c                   eval      *in13 = *on
248900060307     c                   eval      *in99 = *on
249000060307     c                   leavesr
249100060307     c                   endif
249200060307
249300060307      * se tariffa particolare "d " AC BASE la tariffa non deve
249400060307      * avere una tariffa particolare AC PLUS reale con applicazione completa
249500060307     c                   if        vidftc = 'd '
249600060307     c                   movel     'C '          v09ftc
249700061011     c  n12ktpt9         chain(n)  titpt01l
249800061011     c   12ktpt9         chain(n)  tiopt01l
249900061011     c                   if        %found and tptatb = *blanks and
250000060307     c                             tpttma = *blanks and tptapl = *blanks
250100060307     c                   eval      vi2msg = msg(9)
250200060307     c                   eval      *in13 = *on
250300060307     c                   eval      *in99 = *on
250400060307     c                   leavesr
250500060307     c                   endif
250600060307     c                   endif
250700060307
250800060307      * se tariffa particolare "C " AC PLUS reale completa la
250900060307      * tariffa non deve avere una tariffa particolare AC BASE
251000060307     c                   if        vidftc = 'C ' and vi2mas = *blanks and
251100060307     c                             vi2apl = *blanks
251200060307     c                   movel     'd '          v09ftc
251300061011     c  n12ktpt9         chain(n)  titpt01l
251400061011     c   12ktpt9         chain(n)  tiopt01l
251500061011     c                   if        %found and tptatb = *blanks
251600060307     c                   eval      vi2msg = msg(10)
251700060307     c                   eval      *in13 = *on
251800060307     c                   eval      *in99 = *on
251900060307     c                   leavesr
252000060307     c                   endif
252100060307     c                   endif
252200920827     C*
252300920924     C*** ARROTONDAMENTI : TUTTI <> 0  OPPURE  TUTTI = 0
252400941111    1C     VI2ARL        IFEQ      0
252500920611     C*
252600941111    2C     VI2ARF        IFNE      0
252700940712     C     VI2ARO        ORNE      0
252800920611     C                   SETON                                        4299
252900920611     C                   GOTO      ENDCON
253000941111    2C                   END
253100920611     C*
253200941111   X1C                   ELSE
253300940712     C*
253400941111    2C     VI2ARF        IFEQ      0
253500940712     C     VI2ARO        OREQ      0
253600920611     C                   SETON                                        4299
253700920611     C                   GOTO      ENDCON
253800941111    2C                   END
253900920611     C*
254000941111    1C                   END
254100920624     C*
254200950119     C* SE SONO STATI IMMESSI GLI ARROTONDAMENTI CONTROLLO SE SI TRATTA
254300950119     C*   DI SCAGLIONE SENZA DECIMALI PER CUI NON SI POSSONO INSERIRE
254400950119     C*   VALORI DOPO LA VIRGOLA
254500950119    1C     VI2ARL        IFGT      0
254600950119     C     WSCD          ANDEQ     ' '
254700950119     C                   MOVE      VI2ARL        W0030
254800950119    2C     W0030         IFGT      0
254900950119     C                   SETON                                        7299
255000950119     C                   GOTO      ENDCON
255100950119    2C                   ENDIF
255200950119     C*
255300950119     C                   MOVE      VI2ARF        W0030
255400950119    2C     W0030         IFGT      0
255500950119     C                   SETON                                        7299
255600950119     C                   GOTO      ENDCON
255700950119    2C                   ENDIF
255800950119     C*
255900950119     C                   MOVE      VI2ARO        W0030
256000950119    2C     W0030         IFGT      0
256100950119     C                   SETON                                        7299
256200950119     C                   GOTO      ENDCON
256300950119    2C                   ENDIF
256400950119    1C                   ENDIF
256500950119     C*
256600941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
256700941111    1C     *IN11         IFEQ      '1'
256800930223     C*
256900930223     C*** VALORE MERCE FORZABILE A ZERO CON ENTER
257000980309     C* SOLO SE NON SI TRATTA DI UNA TARIFFA IMPORT
257100990217    2C     *IN43         IFEQ      *ON
257200980429     C** SE SI TRATTA DI UNA MANDATO ASSICURATIVO FITTIZIO VI2MAS = N
257300980429     C** IL VALORE MERCE ASSICURABILE DEVE ESSERE A ZERO
257400981126    3C     VI2MAS        IFEQ      'F'
257500980429     C     VI2VLM        ANDGT     0
257600980429     C                   SETON                                        5299
257700980429     C                   GOTO      ENDCON
257800981126    3C                   END
257900981126    2C                   END
258000980309     C*
258100981126    2C     IMPORT        IFNE      'Y'
258200980416     C* CONTROLLO SE TARIFFA ESTERA
258300981126     C   24
258400981126    3COR 25PARFIE        IFEQ      'E'
258500980416     C* SE IL VALORE MERCE E' A ZERO IL TIPO APPLICAZIONE DEVE ESSERE SOLO
258600980416     C* IN PARTENZA
258700980416     C     VI2VLM        ANDEQ     0
258800980416     C     VI2APL        ANDNE     'P'
258900980416     C                   SETON                                        5399
259000980416     C                   GOTO      ENDCON
259100981126    3C                   END
259200980416     C*
259300981126    3C   24UNO           IFEQ      ' '
259400930223     C     VI2VLM        ANDEQ     0
259500980505     C     VI2MAS        ANDEQ     ' '
259600930223     C                   SETON                                        5099
259700930223     C                   MOVEL     '1'           UNO
259800930223     C                   GOTO      ENDCON
259900981126    3C                   END
260000981126   X2C                   ELSE
260100980309     C* VALORE MERCE
260200981126    3C   24VI2VLM        IFEQ      *ZEROS
260300980309     C                   SETON                                        5599
260400981126    3C                   END
260500980309     C* APPLICAZIONE TARIFFA SOLO IN ARRIVO
260600981126     C   24
260700981126    3COR 25VI2APL        IFNE      'A'
260800980309     C                   SETON                                        5499
260900980416     C                   GOTO      ENDCON
261000981126    3C                   END
261100980309     C*
261200981126    2C                   END
261300990817     C* VALORE MERCE VERIFICO SE HA IMPORTI CON DECIMALI
261400990817     C*
261500990817    2C     VI2VLM        IFGT      0
261600990817     C                   MOVE      VI2VLM        W0030             3 0
261700990817    3C     W0030         IFGT      0
261800990817     C     FLGDEC        ANDNE     'S'
261900990817     C                   SETON                                        6799
262000990817     C                   GOTO      ENDCON
262100990817    3C                   END
262200990817    2C                   END
262300921009     C*
262400930219     C*** LIMITE RISARCIBILE NON PUO' ESSERE MINORE DI 12000
262500990907    2C   25VI2VLM        IFLT      §GELRP
262600930219     C                   SETON                                        5699
262700930219     C                   GOTO      ENDCON
262800941111    2C                   END
262900921009     C*
263000920827     C*** TIPO VALORE MERCE
263100920827     C* TIPO VALORE OBBLIGATORIO
263200941111    2C     VI2FVM        IFEQ      '?'
263300980316     C                   MOVEL     'TR'          COD
263400920925     C                   MOVEL     ' '           VI2FVM
263500001020     C                   MOVE      *BLANKS       KEY
263600920925     C                   MOVEL     *BLANKS       DES
263700920925     C                   CALL      'X§TABER'
263800920925     C                   PARM                    CODUT
263900920925     C                   PARM                    COD
264000920925     C                   PARM                    KEY
264100920925     C                   PARM                    DES
264200920925     C                   MOVEL     KEY           VI2FVM
264300920925     C                   MOVEL     DES           DESFVM
264400920925     C                   SETON                                        98
264500920925     C                   GOTO      ENDCON
264600941111    2C                   END
264700920925     C*
264800941111    2C     VI2FVM        IFEQ      ' '
264900920827     C                   SETON                                        5799
265000920827     C                   GOTO      ENDCON
265100941111    2C                   END
265200941111     C*
265300920827     C* CONTROLLO VALIDITA' TIPO VALORE
265400980316     C                   MOVEL     'TR'          COD
265500920827     C                   MOVEL     *BLANKS       KEY
265600920827     C                   MOVEL     VI2FVM        KEY
265700920827     C     KTAB          CHAIN     TABEL                              34
265800920827     C*
265900920827     C* SE TIPO VALORE ANNULLATO : ERRORE
266000941111    2C  N34TBLFLG        IFNE      ' '
266100920827     C                   SETON                                        5899
266200920827     C                   GOTO      ENDCON
266300941111   X2C                   ELSE
266400980316     C                   MOVEL     TBLUNI        DSTR
266500941111     C* CONTROLLO SE E' DA UTILIZZARE PER IL CALCOLO VALORE
266600980316     C     §TRUCV        IFNE      'S'
266700920827     C                   SETON                                        5999
266800920827     C                   GOTO      ENDCON
266900920827     C                   ELSE
267000920827     C* DECODIFICO TIPO VALORE
267100980316     C                   MOVEL     §TRDES        DESFVM
267200920827     C                   END
267300941111    2C                   END
267400920827     C*
267500920827     C* 34 ON - TIPO VALORE MERCE INESISTENTE
267600920827     C   34              SETON                                        5999
267700060802     C                   GOTO      ENDCON
267800941111    1C                   END
267900140219      * FUEL
268000060802      * 82 ON - controllo la data riferimento prezzo base
268100060802     c                   if        *in82
268200140219      * il prezzo non può essere a zero
268300140219     c                   if        vi2pmg = 0
268401140219     c                   eval      *in83 = *on
268403140219     c                   eval      *in99 = *on
268500060802     c                   goto      endcon
268600060802     c                   endif
268700140219      * verifico la correttezza del prezzo richiamndo il TRULPMG
268701140219     c                   clear                   trulpmgds
268702140219     c                   eval      ipmgpmg = vi2pmg
268703140219     c                   clear                   kpjbu
268704140219     c                   eval      kpjbu = Trulpmgds
268705140219     c                   call      'TRULPMGR'
268706140219     c                   parm                    kpjba
268707140219     c                   eval      trulpmgds = kpjbu
268708140219      * scaglione non trovato
268709140219     c                   If        opmgerr = '1'
268710140219     c                   eval      *in84 = *on
268711140219     c                   eval      *in99 = *on
268712140219     c                   eval      vo2sca = *zeros
268713140219     c                   eval      vo2dpb = *zeros
268714140219     c                   goto      endcon
268715140219     c                   endif
268716140219      * prezzo mai rilevato settimanalmente dal Ministero
268717140219     c                   If        opmgerr = '2'
268718140219     c                   eval      *in85 = *on
268719140219     c                   eval      *in99 = *on
268720140219     c                   eval      vo2sca = *zeros
268721140219     c                   eval      vo2dpb = *zeros
268722140219     c                   goto      endcon
268723140219     c                   endif
268724140219     c                   eval      vo2sca = opmgsca
268725140219     c                   eval      vo2dpb = opmgdtd
268726140219     c                   eval      vi2pmg = opmgpmg
272300060802
272400060802     c                   endif
272500920827     C*
272600920611     C     ENDCON        ENDSR
272700921009     C*
272800920624     C*--- AGGIORNO CAMPI DEL SFL ------------------------------------*
272900920610     C     REGIS         BEGSR
273000920709     C*
273100920610     C                   Z-ADD     1             NR2
273200941111     C     NR2           CHAIN     TA67S07                            31
273300920610     C     *IN31         DOWEQ     '0'
273400920616     C     VI2CTS        IFNE      *BLANKS
273500921126     C*
273600921126     C                   SETOFF                                       60
273700921126     C* VI2NRR = 0  SI TRATTA DI RECORD DI NUOVA IMMISSIONE
273800921126     C     VI2NRR        IFEQ      0
273900921216     C                   SETON                                        60
274000921126     C*
274100921126     C                   ELSE
274200921126     C     VI2ATB        IFEQ      ' '
274300921126     C     VI2REC        IFNE      SAVREC
274400921216     C                   SETON                                        60
274500921126     C                   END
274600921126     C                   ELSE
274700921126     C                   SETON                                        60
274800921126     C                   END
274900921126     C                   END
275000921126     C*
275100061011     C   60
275200061011     Cann12KTPD          CHAIN     TITPD000                           33
275300061011     c   60
275400061011     can 12ktpd          chain     tiopd000                           33
275500921126     C   60VI2ATB        IFNE      ' '
275600030715     c                   if        *in33 = *off
275700030715      * non è offerta
275800030715     c                   if        *in12 = *off
275900030715     C                   MOVEL     'A'           TPDATB
276000030715     C                   MOVEL     ' '           TPDFTR
276100040915     C                   Z-ADD     dateu8        TPDDTR
276200030715     C                   UPDATE    TITPD000
276300030716      * verifico prima se esiste l'immissioine di questa tariffa
276400030716     c                   movel(p)  VIDftc        w_manftc
276500030716     c                   move      'I'           W_manftc
276600030716     c                   z-add     1             WW
276700030716     c     w_manftc      lookup    manftc                                 87
276800030715      * mi salvo la tariffa particolare da registrare nello storico variazioni
276900030716      * se non esiste la fase di immissione
277000030716     c                   if        not %found
277100030716      *
277200030715     c                   movel(p)  VIDftc        w_manftc
277300030715     c                   move      'M'           W_manftc
277400030715     c                   z-add     1             WW
277500030715     c     w_manftc      lookup    manftc                                 87
277600030715     c                   if        not %found
277700030715     c     '   '         lookup    manftc(ww)                             87
277800030715     c                   if        %found
277900030715     c                   movea     w_manftc      manftc(ww)
278000030715     c                   endif
278100030715     c                   endif
278200030716      *
278300030716     c                   endif
278400030715      *
278500030715     c                   else
278600030715      * offerta
278700061011     C                   DELETE    tiopd000
278800030715     c                   endif
278900921218     C*
279000030715     c                   endif
279100030715     C*
279200990615     C* AGGANCIO TNTPT PER AGGIORNARE ULTIMA DATA VARIAZIONE CON TRASMISSIONE
279300990615     C* DEL DATO
279400061011     C  n12KTPT          CHAIN     TITPT000                           32
279500061011     c   12ktpt          chain     tiopt000                           32
279600990615     C  N32              CLEAR                   TPTFTR
279700990615     C* DATA ULTIMA VARIAZIONE
279800990615     C  N32              Z-ADD     DATEU8        TPTDUV
279900040915     C  N32              z-add     dateu8        TPTDTR
280000061011     C  N32
280100061011     Cann12              UPDATE    TITPT000
280200061011     c  n32
280300061011     can 12              update    tiopt000
280400990615     C*
280500921218     C*
280600920610     C                   ELSE
280700920611     C* 33 ON  - IMMISSIONE
280800920611     C* 33 OFF - VARIAZIONE
280900941110     C                   Z-ADD     VI2ITR        TPDITR
281000940712     C                   Z-ADD     VI2MIN        TPDMIN
281100940713     C                   Z-ADD     VI2MAX        TPDMAX
281200930217     C*
281300930219     C   06VI2AIN        IFEQ      'N'
281400930219     C                   MOVEL     ' '           TPDAIN
281500930219     C                   ELSE
281600930219     C                   MOVEL     VI2AIN        TPDAIN
281700930219     C                   END
281800930217     C*
281900920611     C                   MOVEL     VI2ATB        TPDATB
282000920617     C                   MOVEL     VI2ORP        TPDORP
282100930217     C  N06              MOVEL     ' '           TPDAIN
282200921214     C                   MOVEL     ' '           TPDFTR
282300920611     C* SOLO PER IMMISSIONE
282400920611     C   33              DO
282500920611     C                   MOVEL     VI2CTS        TPDCTS
282600940712     C                   Z-ADD     VI2SGL        TPDSGL
282700920612     C                   Z-ADD     VIDCLI        TPDKSC
282800920612     C                   Z-ADD     VIDCTR        TPDCTR
282900920612     C                   Z-ADD     VIDPRG        TPDPRG
283000920612     C                   MOVEL     VIDFTC        TPDFTC
283100920610     C                   END
283200980220     C* VALORE POSITIVO NEI CAMPI
283300980220     C                   MLLZO     1             TPDSGL
283400980220     C                   MLLZO     1             TPDITR
283500980220     C                   MLLZO     1             TPDMIN
283600980220     C                   MLLZO     1             TPDMAX
283700920610     C*
283800040915     C                   Z-ADD     dateu8        TPDDTR
283900061011     c                   if        not *in12
284000061011     C  N33              UPDATE    TITPD000
284100061011     C   33              WRITE     TITPD000
284200061011     c                   endif
284300061011     c                   if        *in12
284400061011     c  n33              update    tiopd000
284500061011     c   33              write     tiopd000
284600061011     c                   endif
284700030715      *
284800921217     C*
284900990615     C* AGGANCIO TNTPT PER AGGIORNARE ULTIMA DATA VARIAZIONE CON TRASMISSIONE
285000990615     C* DEL DATO
285100061011     C  n12KTPT          CHAIN     TITPT000                           32
285200061011     c   12ktpt          chain     tiopt000                           32
285300030715     c                   if        *in32 = *off
285400030715     C                   CLEAR                   TPTFTR
285500040915     C                   Z-ADD     DATEU8        TPTDTR
285600990615     C* DATA ULTIMA VARIAZIONE
285700030715     C                   Z-ADD     DATEU8        TPTDUV
285800061011     C  n12              UPDATE    TITPT000
285900061011     c   12              update    tiopt000
286000030715      * mi salvo la tariffa particolare da registrare nello storico variazioni
286100030715      * se non sono in offerta
286200030715     c                   if        *in12 = *off
286300030715      *
286400030715     c                   movel(p)  VIDftc        w_manftc
286500030715     c                   move      'M'           W_manftc
286600030715     c                   z-add     1             WW
286700030715     c     w_manftc      lookup    manftc                                 87
286800030715     c                   if        not %found
286900030715     c     '   '         lookup    manftc(ww)                             87
287000030715     c                   if        %found
287100030715     c                   movea     w_manftc      manftc(ww)
287200030715     c                   endif
287300030715     c                   endif
287400030715     C*
287500030715     c                   endif
287600990615     C*
287700030715     c                   endif
287800921126     C*
287900920610     C                   END
288000920616     C                   END
288100920616     C                   ADD       1             NR2
288200941111     C     NR2           CHAIN     TA67S07                            31
288300920610     C                   END
288400920610     C                   ENDSR
288500920610     C*
288600920624     C*--- AGGIORNO CAMPI DEL SFLCTL ---------------------------------*
288700920624     C     REGIS2        BEGSR
288800920709     C*
288900920624     C* 32 ON  - IMMISSIONE
289000920624     C* 32 OFF - VARIAZIONE
289100061011     C  n12KTPT          CHAIN     TITPT000                           32
289200061011     c   12ktpt          chain     tiopt000                           32
289300920624     C                   MOVEL     VI2TPG        TPTTPG
289400940712     C                   Z-ADD     VI2ARL        TPTARL
289500920624     C                   Z-ADD     VI2ARF        TPTARF
289600920624     C                   Z-ADD     VI2ARO        TPTARO
289700920624     C                   Z-ADD     VI2RPV        TPTRPV
289800920709     C                   MOVEL     ' '           TPTATB
289900990915     C                   MOVEL     VI2MAS        TPTTMA
290000060802     c                   If        *in82
290100140219     C                   Z-ADD     vo2dpb        TPTDPB
290200140115     c                   eval      §TPTpma = VI2pma
290300060829     c                   else
290400060829     c                   clear                   tptdpb
290500140115     c                   clear                   §TPTpma
290600060803     c                   endif
290700060803      *
290800060803     c   32              z-add     dateu8        TPTdit
290900941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
291000941111    1C     *IN11         IFEQ      *ON
291100941111     C                   Z-ADD     VI2VLM        TPTVLM
291200941111     C                   MOVEL     VI2FVM        TPTFVM
291300941111   X1C                   ELSE
291400941111     C                   Z-ADD     0             TPTVLM
291500941111     C                   MOVEL     ' '           TPTFVM
291600941111    1C                   ENDIF
291700941111     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
291800941111    1C     *IN28         IFEQ      *ON
291900941111     C                   MOVEL     VI2APL        TPTAPL
292000941111   X1C                   ELSE
292100941111     C                   MOVEL     ' '           TPTAPL
292200941111    1C                   ENDIF
292300941111     C*
292400920624     C* SOLO PER IMMISSIONE
292500941111    1C   32              DO
292600920624     C                   Z-ADD     VIDCLI        TPTKSC
292700920624     C                   Z-ADD     VIDCTR        TPTCTR
292800920624     C                   Z-ADD     VIDPRG        TPTPRG
292900920624     C                   MOVEL     VIDFTC        TPTFTC
293000941111    1C                   END
293100941111     C*
293200990614     C* SE VIENE VARIATO TITPT ABBLENCO FLAG E DATA TRASMISSIONE
293300941111    1C     COMTPT        IFNE      TPTDS
293400921215     C                   MOVEL     ' '           TPTFTR
293500040915     C                   Z-ADD     dateu8        TPTDTR
293600921217     C*
293700990903     C*
293800990615     C* DATA ULTIMA VARIAZIONE
293900990615     C                   Z-ADD     DATEU8        TPTDUV
294000941111    1C                   END
294100980220     C* VALORE POSITIVO NEI CAMPI
294200980220     C                   MLLZO     1             TPTARL
294300980220     C                   MLLZO     1             TPTARF
294400980220     C                   MLLZO     1             TPTARO
294500980220     C                   MLLZO     1             TPTRPV
294600980220     C                   MLLZO     1             TPTVLM
294700921215     C*
294800040915     C                   Z-ADD     dateu8        TPTDTR
294900140115     c                   eval      TPTflo = dTPT01
295000061011     c                   if        not *in12
295100990614     C  N32              UPDATE    TITPT000
295200990614     C   32              WRITE     TITPT000
295300061011     c                   endif
295400061011     c                   if        *in12
295500061011     c  n32              update    tiopt000
295600061011     c   32              write     tiopt000
295700061011     c                   endif
295800030715      *
295900030715      * se non sono in offerta salvo il codice tariffa particolare  da scrivere
296000030715      * nello storico
296100030715      *
296200030715     c                   IF        *in12 = *off
296300030715      *
296400030715     c                   movel(p)  VIDftc        w_manftc
296500030715     c  n32              move      'M'           W_manftc
296600030715     c   32              move      'I'           W_manftc
296700030715     c                   z-add     1             WW
296800030715     c     w_manftc      lookup    manftc                                 87
296900030715     c                   if        not %found
297000030715     c     '   '         lookup    manftc(ww)                             87
297100030715     c                   if        %found
297200030715     c                   movea     w_manftc      manftc(ww)
297300030715     c                   endif
297400030715     c                   endif
297500921126     C*
297600030715     c                   endif
297700030715     c*
297800930318     C*
297900930318     C                   SETON                                        0207
298000930318     C                   SETOFF                                       01
298100930223     C*
298200930223     C                   MOVEL     VI2TPG        SAVTPG            1
298300930223     C                   Z-ADD     VI2VLM        SAVVLM            9 0
298400920624     C                   ENDSR
298500131218     C*
298600131218     C*--- Recupero prezzo del gasolio corrispondente ----------------*
298700131218     C     REC_pmg       BEGSR
298800131218     C*
298900131218     c     Kpmg          setll     tipmg01l
299000131218     c                   read      tipmg01l
299100131218     c                   if        %found(tipmg01l)
299200131218      * devo controllare la data del prezzo medio che ho trovato
299300131218      * se la data del prezzo medio è troppo indietro non devo visualizzare
299400131218      * il prezzo (per ora faccio il controllo con 6 gg)
299500131218     c                   if        pmgdtd > *zeros
299600131218     c     *iso          move      pmgdtd        data_iso1
299700131218     c                   else
299800131218     c                   eval      data_iso1 = *loval
299900131218     c                   endif
300000131218     c     *iso          move      Kpmg          data_iso2
300100131218     c     data_iso2     subdur    data_iso1     wgio:*d
300200131218     c                   if        wgio > 6
300300140219     c                   clear                   vI2pmg
300301140219     c                   clear                   vO2sca
300400131218     c                   else
300500140219     c                   eval      vI2pmg = pmgpmg
300501140219     c                   eval      vO2sca = pmgsca
300600131218     c                   endif
300700131218     c                   endif
300800131218     C                   ENDSR
300900131218     C*
301000120320
301100120320      /free
301200120320       //--------------------------------------------------------------
301300120320       //?Visualizza Dati manca tariffa-
301400120320       //--------------------------------------------------------------
301500120320       BEGSR GesW01;
301600120320
301700120320         wFine = *off;
301800120320         W01dsp = ISB48dsp;
301900120320         W01tbl = ISB48tbl;
302000120320         W01lnp = ISB48lnp;
302100120320         W01lna = ISB48lna;
302200120320         W01pro = ISB48pro;
302300120320         W01cts = ISB48cts;
302400120320         W01ctr = ISB48ctr;
302500120320         W01ncl = ISB48ncl;
302600120320         W01pkf = ISB48pkf;
302700120320         W01vlf = ISB48vlf;
302800120320         W01err = ISB48err;
302900120320
303000120320         DOW wFine = *off;
303100120320           exfmt  TA67W01;
303200120320           //?- F12 = Ritorno
303300120320           IF  *inkl;
303400120320             wFine = *on;
303500120320           ENDIF;
303600120320         ENDDO;
303700120320
303800120320       ENDSR;
303900120320      /end-free
304000950119     C*---------------------------------------------------------------*
304100020207**  MS1
304200950119E' possibile inserire solo il Codice Tassazione "xx"                           1
304300950119**  MS2
304400950119E' possibile inserire solo i Codici Tassazione "xx" e "xx"                     1
304500011022**  MSG
304600050531Non è possibile inserire il Codice Tassazione "EE"                             1
304700050531Codice tassazione non utlizzabile per tariffa FedEx                            2
304800050531Codice tassazione utilizzabile solo per tariffe FedEx                          3
304900050531Non è possibile inserire il Codice Tassazione "IT"                             4
305000050531Impossibile inserire la Tar.Particolare "XX" che sarà eliminata il xx/xx/xx    5
305100050704Inserire solo uno scaglione per codice di tassazione!!                         6
305200140128Cliente " " l'importo non può essere inferiore a Euro                          7
305300060307Impossibile inserire AC BASE. Il cliente ha rinunciato al mandato ass. AC BASE 8
305400060307Impossibile inserire AC BASE. Presente AC PLUS reale con applicazione completa 9
305500060307Impossibile inserire AC PLUS reale con applicazione completa. Presente AC BASE 1
305600060705L'importo non può essere inferiore a xxxxxxxx,xxx Euro                        11
305700070226Codice tassazione non utilizzabile                                            12
305900140219                                                                              13
