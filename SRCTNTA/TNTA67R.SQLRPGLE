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
002400140219      *
002500950112     D*
002600950119     D MS1             S              1    DIM(79) CTDATA PERRCD(79)
002700950119     D MS2             S              1    DIM(79) CTDATA PERRCD(79)
002800060705     D MSG             S             79    DIM(20) CTDATA PERRCD(1)
002900950112     D* SCHIERE RELATIVE ALLA TABELLA "TR"
003000990614     D WTT             S              2    DIM(50)
003100920609     D TTR             S              1    DIM(50)
003200980428     D DTR             S             10    DIM(50)
003300941110     D TTP             S              1    DIM(50)
003400950119     D TSP             S              1    DIM(50)
003500980429     D ARR             S              1    DIM(50)
003600980429     D RPV             S              1    DIM(50)
003700950112     D* SCHIERE RELATIVE ALLA TABELLA "15"
003800950112     D NAZ             S              3    DIM(20)
003900080609      * schiera filiali abilitate al lasciato avviso
004000080609     d sklav           s              3  0 dim(999)
004100080609
004200050531     D TParEli         S              2    DIM(20)
004300050531     D TParDat         S              8  0 DIM(20)
004400021209
004500030715     d MANftc          s              3    dim(150)
004600030715     d W_manftc        s              3
004700030715     d WW              s              3  0 inz
004800051012     d Kscfil          s              3  0
004900080609
005000080609     d xx              s              3  0
005100131218
005200131218     d wgio            s              5  0
005300060801
005400060801     D Wdataeur        S               D   DATFMT(*eur)
005500060801     D Wdataoggi       S               D   DATFMT(*eur)
005600060802     d Datamax         s              8  0
005700131218     d data_iso1       s               d   datfmt(*iso)
005800131218     d data_iso2       s               d   datfmt(*iso)
005900030715
006000021209     d Wfqt            s                   Like(§1pFqt)
006100050531     d
006200050531     d wdatadmy        s               d   datfmt(*dmy)
006300050704
006400050704     d savcts          s                   like(tpdcts)
006500061011     d codut           s              1  0 Inz(1)
006600060306     d kcc             s                   like(clpkcc) inz(151)
006700060306     d ksc             s                   like(clpksc)
006800021209
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
015000140219      *
015100990217     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015200150217      *
015300150219      * -?Dati V.M.A. negato?
015400150219     d dVMA          e ds                  inz
015500150217
015600030718     D TNTA04DS      E DS
015700061011
015800061011     d Azuteds       e ds                  extname(Azute00f)
015900061011     d dDatiute      e ds
016000061011     d Tibs34ds      e ds
016100130125     d dUTE01        e ds
016200061011
016300061011     d Tnta25ds      e ds
016400061011     d Tnta25tad     e ds                  inz
016500061011     d Tnta25tgc     e ds                  inz
016600061011     d Tnta25tpd     e ds                  inz
016700061011     d Tnta25tpt     e ds                  inz
016800080609
016900080609     d dlav          e ds
017000080609     d tntbeds       e ds                  extname(tntbe00f)
017100120320
017200120320       // -?ds dati passati da TNSB48R - Manca Tariffa
017300120320     d TNSB48ds      e ds
017400120320     d Parm48          s                   like(tnsb48ds)
017500120713
017600120713       // -?Passaggio dati a TRTB09R - Ricerca codici tassazione
017700120713     d TRTB09DS      e ds
017800140115
017900140219       // -?Passaggio dati a TRULPMGR - Controllo Prezzo base di riferimento
018000140219     d TRULPMGDS     e ds
018100140219
018200140220       // -?Interrogazione e recupero Prezzi medi del Gasolio
018300140220     d TNTA39DS      e ds
018400140220
018500140115       // -?DS campo TPTflo
018600140115     d dTPT01        e ds
018700140115
018800140115       // -?Costante per controllo campi numerici
018900140115     d c_Digits        c                   const('0123456789')
019000090916
019100090916     itivis000
019200090916     i              visesi                      wisesi
019300090916
019400920529     C*****************************************************************
019500920529     C* RIEPILOGO INDICATORI
019600920529     C*****************************************************************
019700920609     C* 01    - IMMISSIONE
019800920609     C* 02    - VARIAZIONE
019900920618     C* 03    - USATO IN VARIAZIONE PER POSIZIONARMI SU CODICE TASSAZ.
020000920618     C* 04    - USATO IN VARIAZIONE PER POSIZIONARMI SU IMPORTO TARIFFA
020100920609     C* 05    - USATO IN VARIAZIONE PER PROTEGGERE I CAMPI
020200920609     C* 06    - TIPO CONSEGNA = ISOLE MINORI
020300920619     C* 07    - PREMUTO CMD6-AGGIORNAMENTO
020400920624     C* 08    - ROLLUP
020500920618     C* 09    - PREMUTO CMD16-ANNULLAMENTO
020600920707     C* 10    - USATO IN INTERROGAZIONE PER PROTEGGERE I CAMPI
020700941111     C* 11    - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
020800920619     C* 12    - PROVENGO DA OFFERTA
020900950119     C* 13    - INDICATORE DI EMISSIONE CAMPO MESSAGGIO DI ERRORE
021000941110     C* 14    - INDICA TIPO TARIFFA/TIPO PAGAMENTO A VALORE
021100920616     C* 15/19 - TASTI DI DUPLICA
021200920707     C* 20    - LOKUP
021300981016     C* 21    - ON  PER IL TIPO CONSEGNA  RCV
021400981016     C*         PROTEGGO IL CAMPO TIPO VALORE.DEVE ESSERE SEMPRE A KG
021500920625     C* 22    - USATO IN ASSICURAZIONE X CONTO PER PROTEGGERE CAMPI
021600920625     C*         DEL FORMATO
021700941111     C* 23    - TASTO DI DUPLICA
021800990217     C* 24    - TARIFFE PARTIC. ASSICURAZIONE X CONTO
021900990217     C* 25    - TAIRFFE PARTIC. RCV
022000920827     C* 26    - USATO X CONDIZIONARE L'EMISSIONE DEGLI ARROTONDAMENTI
022100941110     C* 27    - USATO X CONDIZIONARE L'EMISSIONE DEL   RAPPORTO P/VOL.
022200941111     C* 28    - VISUALIZZO CODICE APPLICAZIONE
022300921007     C* 29    - USATO PER AZZERARE I CAMPI DEL SFLCONTROL NEL CASO IN
022400941110     C*         CUI VENGA VARIATO IL TIPO TARIFFA/TIPO PAGAMENTO
022500920609     C* 30/34 - DI COMODO
022600920609     C* 35/37 - PULIZIA SUBFILE
022700920609     C* 36    - SFLNXTCHG
022800941116     C* 38    - VISUALIZZO IL MINIMO  TARIFFA
022900941116     C* 39    - VISUALIZZO IL MASSIMO TARIFFA
023000941111     C* 40/42 - ERRORE
023100980514     C* 43    - VISUALIZZO IL FLAG MANDATO ASSICURATIVO FITTIZIO
023200990217     C*         SOLO EPR "C" -ASS/C
023300000918     C* 44    - SI TRATTA DI TARUFFA DI CARTELLO PROTEGGO LA COPIA TARIFFE DI
023400000918     C*         CARTELLO
023500941111     C* 45/51 - ERRORE
023600980429     C* 52/59 - ERRORE
023700921126     C* 60    - ON  SIGNIFICA CHE DEVO REGISTRARE IL RECORD
023800941111     C* 61    - USATO PER EMETTERE IL MESSAGGIO DEL CMD14 DI FORZATURA
023900031003     C* 62    - ON SIGNIFICA CHE NON C'E' DETTAGLIO TARIFFE PARTICOLARI
024000990622     C* 63/65 - ERRORE
024100990622     C* 66/73 - ERRORE
024200950119     C* 74    - POSIZIONAMENTO CURSORE SUL CODICE TASSAZIONE PER
024300131217      * 75    - Errore data prezzo base FUEL
024400950119     C*         EMISSIONE CAMPO MESSAGGIO
024500060303     C* 76/77 - ERRORE VI2CTS
024600060303      * 78    - VARIA "d" AC BASE
024700060306      * 79    - Posizionamento cursore
024800000829     C* 80/81 - PULIZIA SUBFILE
024900140219      * 82/85 - Tariffa Particolare "f" Supplemento carburante
025000140219      * 86    - Posizione Cursore Tariffa Particolare "f" Suppl.carbur.
025100140131      * 88    - Tariffa particolare "f" deve avere scaglione >= 999
025200090916      * 89    - Trattativa
025300150219      * 90    - Visualizza "Applicazione VMA (N=No)"
025400120320      * 97    - Pgm richiamato x manca tariffa TNSB48
025500920611     C* 98    - DI COMODO
025600920609     C* 99    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
025700920529     C*****************************************************************
025800920529     C     *ENTRY        PLIST
025900920529     C                   PARM                    KPJBA
026000030715     C                   PARM                    manftc
026100120320     c                   parm                    Parm48
026200920615     C                   MOVEL     KPJBU         PARAM5
026300080609
026400080609     c/exec sql
026500080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
026600080609     c/end-exec
026700061011
026800061011     c     *dtaara       define    §azute        azuteds
026900061011     c     *dtaara       define    §datiute      ddatiute
027000061011     c                   in(E)     *dtaara
027100061011     c                   If        %error  or RSUT = *blanks
027200061011     c                   Clear                   Tibs34ds
027300061011     c                   Call      'TIBS34R'
027400061011     c                   Parm                    Tibs34ds
027500061011     c                   In        *dtaara
027600061011     c                   EndIf
027700120320
027800120320
027900120320      /free
028000120320       //?controllo se passati tutti i parametri
028100120320         IF  %parms < 3;
028200120320           clear tnsb48ds;
028300120320           *in97 = *off;
028400120320         ELSE;
028500120320           clear tnsb48ds;
028600120320           tnsb48ds = parm48;
028700120320           *in97 = *on;
028800120320         ENDIF;
028900120320
029000120320         clear OSB48err;
029100120320         clear OSB48msg;
029200120320      /end-free
029300120320
029400120320
029500990615     C* PRENDO LA DATA DEL GIORNO DA TIME
029600990615     C                   TIME                    W0140            14 0
029700990615     C                   MOVE      W0140         UDATE8            8 0
029800990615     C*
029900990615     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
030000990615     C*
030100060802     C                   Z-ADD     UDATE8        G08DAT
030200060802     C                   MOVEL     *BLANK        G08ERR
030300990615     C                   CALL      'XSRDA8'
030400990615     C                   PARM                    WLBDAT
030500990615     C* UDATE A 8 IN AAAA/MM/GG
030600060802     C                   Z-ADD     G08INV        DATEU8            8 0
030700060801     C     *iso          movel     dateu8        Wdataoggi
030800060801      * calcolo la data massima di controllo per supplemento carburante
030900060803     c     Wdataoggi     adddur    06:*m         Wdataeur
031000060802     c     *iso          movel     Wdataeur      Datamax
031100090916
031200090916      * controllo se richiamato da trattativa
031300090916     c                   if        partrat = 'T'
031400090916     c                   eval      *in89 = *on
031500090916     c                   else
031600090916     c                   eval      *in89 = *off
031700090916     c                   endif
031800920529     C*---------------------------------------------------------------*
031900990614     C* ACCESSO TITPT01L/TITPD01L
032000920612     C     KTPT          KLIST
032100920612     C                   KFLD                    VIDCLI
032200920612     C                   KFLD                    VIDCTR
032300920612     C                   KFLD                    VIDPRG
032400920612     C                   KFLD                    VIDFTC
032500000901     C* ACCESSO A TPT/TPD DA SFL COPIA DA CARTELLO
032600000901     C     KTPT9         KLIST
032700000901     C                   KFLD                    VIDCLI
032800000901     C                   KFLD                    VIDCTR
032900000901     C                   KFLD                    VIDPRG
033000000901     C                   KFLD                    V09FTC
033100990614     C* ACCESSO TITPD01L
033200920612     C     KTPD          KLIST
033300920612     C                   KFLD                    VIDCLI
033400920612     C                   KFLD                    VIDCTR
033500920612     C                   KFLD                    VIDPRG
033600920612     C                   KFLD                    VIDFTC
033700920612     C                   KFLD                    VI2ORP
033800940712     C                   KFLD                    VI2SGL
033900050704      *
034000050704     c     ktpd01        klist
034100050704     c                   kfld                    vidcli
034200050704     c                   kfld                    vidctr
034300050704     c                   kfld                    vidprg
034400050704     c                   kfld                    vidftc
034500050704     c                   kfld                    vi2orp
034600940712     C* ACCESSO TABEL00F
034700920608     C     KTAB          KLIST
034800920608     C                   KFLD                    CODUT
034900940712     C                   KFLD                    COD
035000940712     C                   KFLD                    KEY
035100920608     C     KTAB2         KLIST
035200920608     C                   KFLD                    CODUT
035300920608     C                   KFLD                    COD
035400060306
035500060306      * cnclp00f/tfclp00f
035600060306     c     kclp          klist
035700060306     c                   kfld                    codut
035800060306     c                   kfld                    kcc
035900060306     c                   kfld                    ksc
036000140128
036100140128      * TIACB01L
036200140128     c     kACB          klist
036300140128     c                   kfld                    CLPclv
036400140128     c                   kfld                    dateu8
036500140128
036600940712     C***
036700940712     C** DEFINIZIONE CAMPI
036800940712     C***
036900940712     C     *LIKE         DEFINE    TBLCOD        COD
037000940712     C     *LIKE         DEFINE    TBLKEY        KEY
037100941109     C     *LIKE         DEFINE    §1PCTS        WCTS
037200941123     C     *LIKE         DEFINE    §1PCTS        WCTE
037300941109     C     *LIKE         DEFINE    §1PPTV        WPTV
037400940712     C* CAMPI DI DUPLICA
037500940712     C     *LIKE         DEFINE    TPDCTS        DUPCTS
037600940712     C     *LIKE         DEFINE    TPDSGL        DUPSGL
037700941110     C     *LIKE         DEFINE    TPDITR        DUPITR
037800940712     C     *LIKE         DEFINE    TPDMIN        DUPMIN
037900940712     C     *LIKE         DEFINE    TPDMAX        DUPMAX
038000940712     C     *LIKE         DEFINE    TPDAIN        DUPAIN
038100940712     C*
038200920529     C*---------------------------------------------------------------*
038300930223     C***** REPERISCO IL LIMITE RISARCIBILE STABILITO PER LEGGE  *****
038400990907     C*****   DALLA TABELLA GEN PASSANDOGLI LA MONETA            *****
038500990907      *
038600990907     C                   CLEAR                   DSBS02
038700990907     C                   CLEAR                   DGEI
038800990907     C                   MOVEL     'C'           T02MOD
038900990907     C                   MOVEL     KNSIF         T02SIF
039000990907     C                   MOVEL     'GEI'         T02COD
039100990907     C                   MOVEL     VICDIV        T02KE1
039200990907      *
039300990907     C                   CALL      'TIBS02R'
039400990907     C                   PARM                    KPJBA
039500990907     C                   PARM                    DSBS02
039600990907      *
039700990907     C     T02ERR        IFEQ      *BLANKS
039800990907     C                   MOVEL     T02UNI        DGEI
039900990907     C                   ENDIF
040000920609     C*
040100980317     C* CARICO TABELLA CODICI NAZIONE
040200980317     C                   EXSR      CARNAZ
040300080609
040400080609      * carico tabella filiali attive al lasciato avviso
040500080609     c                   exsr      carlav
040600920619     C*
040700920619     C* CONTROLLO SE PROVENGO DA OFFERTA
040800920619     C     FLG           IFNE      ' '
040900920619     C                   Z-ADD     VIDCLI        VIDNRV
041000090916     C                   Z-ADD     VIDCLI        VIDNRVT
041100920619     C                   SETON                                        12
041200110103     c**n89              open      tnvis05l
041300090916     c   89              open      tivis05l
041400110103     c**n89vidnrv        chain     tnvis05l
041500090916     c   89vidnrv        chain     tivis05l
041600090916     c                   If        %found
041700051012     c                   movel     viscmm        kscfil
041800051012     c                   else
041900051012     c                   clear                   kscfil
042000051012     c                   endif
042100110103     c**n89              close     tnvis05l
042200090916     c   89              close     tivis05l
042300060306     c                   open      tfclp00f
042400111122     c                   open      cnclp00f
042500061011
042600061011     c                   open      tiopt01l
042700061011     c                   open      tiopd01l
042800061011
042900051012     c                   else
043000051012      * no offerta
043100051012     c                   movel     vidcli        kscfil
043200060306     c                   open      cnclp00f
043300061011     c                   open      titpt01l
043400061011     c                   open      titpd01l
043500061011     c                   setoff                                       12
043600920619     C                   END
043700990217     C**
043800040429      * verifico codice cliente se inizia con 88888 si tratta di CARTELLO
043900040429     c                   movel     vidcli        w0050             5 0
044000040429     c                   if        w0050 = 88888
044100040429     c                   seton                                        44
044200040429     c                   endif
044300060306      * se non è tariffa di cartello recupero il cod.importanza del CLIENTE
044400060306      * o della VISITA
044500060306     c                   if        not *in44
044600060306     c                   move      vidcli        ksc
044700060306     c  n12kclp          chain     cnclp00f
044800060306     c   12kclp          chain     tfclp00f
044900111122     c                   If        *in12 and not %found(tfclp00f)
045000111122      * se non trovata l'anagrafica provvisoria aggancio l'anagrafica del client
045100111122      * e della trattativa
045200111122     c                   move      visksc        Ksc
045300111122     c     kclp          chain     cnclp00f
045400111122     c                   endif
045500111121     c                   If        clpclv = ' '
045600111121     c                   eval      clpclv = 'C'
045700111121     c                   Endif
045800060306     c                   endif
045900130125
046000130125       //?Imposto flag x bloccare le tariffe particolari in manutenzione
046100130125       //?ma solo TARIFFE e NO CARTELLO
046200130125     c                   eval      dUTE01 = UTEfaf
046300130125     c                   IF        not *in12 and not *in44
046400130125     c                             and §UTEmodtar = *blanks
046500130125     c                   eval      wBlocca = *on
046600130125     c                   ENDIF
046700000829     C*
046800000829     C     CARTAG        TAG
046900921126     C*
047000920529     C* CARICO SUBFILE
047100920529     C                   EXSR      CARICA
047200920609     C* SE NON CARICA NIENTE ESCO
047300920529     C     NRR           IFEQ      0
047400920529     C                   GOTO      FINE
047500920529     C                   END
047600920529     C*
047700920529     C     INIZIO        TAG
047800940913     C                   WRITE     TA67T01
047900940913     C                   WRITE     TA67Z03
048000940913     C                   EXFMT     TA67C02
048100050531     c
048200050531     c                   setoff                                       4013
048300920529     C*
048400921117     C** CMD3  - FINE LAVORO
048500921117     C   KC              MOVEL     'A'           FLG2
048600921118     C   KC
048700921117     C** CMD12 - RITORNO
048800921117     COR KL              GOTO      FINE
048900030718     C** CMD07 - INTERROGAZIONE STORICO VARIAZIONI
049000030718     c                   if        *inkg
049100030718     c                   clear                   tnta04ds
049200030718     c                   eval      i04ksc = vidcli
049300030718     c                   eval      i04ctr = vidctr
049400030718     c                   eval      i04prg = vidprg
049500030718     c                   eval      i04rag = ragdes
049600030718     c                   eval      i04dct = viddcf
049700030718     c                   eval      i04dcv = viddcv
049800030718     c                   eval      i04div = vicdiv
049900030718     c                   eval      i04ndf = flgnet
050000030718      *
050100030718     c                   call      'TNTA04R'
050200030718     c                   parm                    tnta04ds
050300030718      *
050400030718     c                   goto      inizio
050500030718      *
050600030718     c                   endif
050700120320       //?F1 - dati manca tariffa
050800120320     c                   IF        *inka
050900120320     c                   exsr      gesw01
051000120320     c                   goto      inizio
051100120320     c                   ENDIF
051200030718     C*
051300000829     C** CMD13 - COPIA TARIFFE PARTICOLARI DA CARTELLO
051400000829     C     *INKM         IFEQ      *ON
051500000829     C                   EXSR      COPCAR
051600000829     C                   GOTO      CARTAG
051700000829     C                   ENDIF
051800000829     C**
051900920529     C* CONTROLLO DATI SUBFILE
052000920529     C                   SETOFF                                       31
052100940913     C                   READC     TA67S02                                31
052200921118     C   31              GOTO      FINE
052300050531    1C     *IN31         DOWEQ     '0'
052400980317     C*
052500980317     C                   SETOFF                                       0709
052600050531     c                   z-add     1             rec
052700920708     C*
052800050531    2C     VIDSCE        IFEQ      '1'
052900050531     c                   clear                   wfqt
053000050531     c* se tariffa non essitente, non la faccio inserire se in scadenza,
053100050531     c*  da eliminare
053200050531    3c                   if        vidflg=' ' and viheli>*zeros and
053300050531     c                             pardst>=viheli
053400050531     c                   eval      vidmsg=msg(5)
053500050531     c                   eval      %subst(vidmsg:42:2)=vidftc
053600050531     c                   movel     viheli        wdata8            8 0
053700050531     c     *iso          move      wdata8        wdatadmy
053800050531     c                   move      wdatadmy      wdata6            6 0
053900050531     c                   eval      %subst(vidmsg:68:8) =(%editw(wdata6:'  /  / -
054000050531     c                              '))
054100050531     c                   seton                                        13
054200050531    3c                   endif
054300060306
054400060306      * se immissione della tariffa particolare "d " AC BASE la tariffa non deve
054500060306      * avere il flag di rinuncia AC BASE
054600060306     c                   if        not *in13 and vidflg = *blanks and
054700060306     c                             vidftc = 'd ' and parfmp = 'S'
054800060306     c                   eval      vidmsg = msg(8)
054900060306     c                   eval      *in13 = *on
055000060306     c                   endif
055100060306
055200060306      * se immissione della tariffa particolare "d " AC BASE la tariffa non deve
055300060306      * avere una tariffa particolare AC PLUS reale con applicazione completa
055400060306     c                   if        not *in13 and vidflg = *blanks and
055500060306     c                             vidftc = 'd '
055600060306     c                   movel     'C '          v09ftc
055700061011     c  n12ktpt9         chain(n)  titpt01l
055800061011     c   12ktpt9         chain(n)  tiopt01l
055900061011     c                   if        %found and tptatb = *blanks and
056000060306     c                             tpttma = *blanks and tptapl = *blanks
056100060306     c                   eval      vidmsg = msg(9)
056200060306     c                   eval      *in13 = *on
056300060306     c                   endif
056400060306     c                   endif
056500130104
056600130104      * se scelta della tariffa particolare "f " Fuel Surcharge errore
056700130104      * in caso di manutenzione e tariffa, la cartello è libera
056800130128     c*******            IF        not *in13 and vidflg <> *blanks and
056900130128     c*******                      not *in12 and not *in44 and vidftc = 'f '
057000130128     c*******            eval      vidmsg = 'Impossibile manutenzionare la -
057100130128     c*******                      Tariffa Particolare Fuel Surcharge'
057200130128     c*******            eval      *in13 = *on
057300130128     c*******            ENDIF
057400970401     C**
057500050531    3c                   if        not *in13
057600920529     C                   MOVEL     VIDFTC        VI2FTC
057700920612     C                   MOVEL     VIDDES        VI2DTC
057800980317     C* PULIZIA DELLE SCHIERE CODICI TARIFFE PER LA SINGOLA TARIFFA PARTICOLARE
057900980317     C                   MOVEL     *BLANKS       TTR
058000980317     C                   MOVEL     *BLANKS       DTR
058100980317     C                   MOVEL     *BLANKS       TTP
058200980317     C                   MOVEL     *BLANKS       TSP
058300980317     C                   MOVEL     *BLANKS       WTT
058400980429     C                   MOVEL     *BLANKS       ARR
058500980429     C                   MOVEL     *BLANKS       RPV
058600920831     C*
058700941020     C* CARICO I TIPI TARIFFA VALIDI PER LA CONS.PARTICOL. SELEZIONATA
058800941020     C                   MOVEL     '1P'          COD
058900941020     C                   MOVEL     *BLANKS       KEY
059000941020     C                   MOVEL     VIDFTC        KEY
059100941020     C     KTAB          CHAIN     TABEL                              34
059200050531    4C     *IN34         IFEQ      *OFF
059300941109     C* UTT --> TIPI TARIFFA VALIDI PER LA CONSEGNA SELEZIONATA        A
059400980428     C                   MOVEL     TBLUNI        DS1P
059500130125
059600130125       //?Se non posso manutenzionare le tariffe ed è una tariffa
059700130125       //?particolare NON sbloccata errore
059800130125     c                   IF        wBlocca and §1Pmodtp = *blanks
059900130125     c                   eval      vidmsg = 'Impossibile gestire la Tariffa -
060000130125     c                             Particolare selezionata'
060100130125     c                   eval      *in13 = *on
060200130125     c                   goto      nomantp
060300130125     c                   ENDIF
060400130125
060500980428     C                   MOVEL     §1PTT0        WTT(1)
060600980428     C                   MOVEL     §1PTT1        WTT(2)
060700980428     C                   MOVEL     §1PTT2        WTT(3)
060800980428     C                   MOVEL     §1PTT3        WTT(4)
060900980428     C                   MOVEL     §1PTT4        WTT(5)
061000980428     C                   MOVEL     §1PTT5        WTT(6)
061100980428     C                   MOVEL     §1PTT6        WTT(7)
061200980428     C                   MOVEL     §1PTT7        WTT(8)
061300980428     C                   MOVEL     §1PTT8        WTT(9)
061400980428     C                   MOVEL     §1PTT9        WTT(10)
061500980428     C                   MOVEL     §1PTTV        WTT(11)
061600980317     C*
061700980317     C* CARICO TABELLA CODICI TARIFFA
061800980317     C                   EXSR      CARTAB
061900941020     C*
062000941109     C* WCTS --> SE PIENO, IL DETTAGLIO DELLA TARIFFA PARTICOLARE DEVE
062100941109     C*          AVERE COME CODICE TASSAZ. IL CODICE CONTENUTO IN WCTS
062200050531    5C     PARFIE        IFEQ      'I'
062300941123     C* SE TARIFFA ITALIA DEVO PRENDERE COD.TASS. ITALIA
062400941109     C                   MOVEL     §1PCTS        WCTS
062500941123     C                   CLEAR                   WCTE
062600941109     C                   ELSE
062700050531    6C     PARFIE        IFEQ      'E'
062800941123     C* SE TARIFFA ESTERO DEVO PRENDERE COD.TASS. ESTERO
062900941109     C                   MOVEL     §1PCTE        WCTS
063000941123     C                   CLEAR                   WCTE
063100941109     C                   ELSE
063200941123     C* SI TRATTA DI TARIFFA DI CARTELLO QUINDI POSSONO ESISTERE
063300941123     C*   ENTRAMBI I CODICI TASSAZIONE ESPRESSI IN TABELLA
063400941123     C                   MOVEL     §1PCTS        WCTS
063500941123     C                   MOVEL     §1PCTE        WCTE
063600050531    6C                   ENDIF
063700050531    5C                   ENDIF
063800941109     C*
063900941109     C* SALVO LA MASSIMA % PER TARIFFA A VALORE
064000941109     C                   MOVEL     §1PPTV        WPTV
064100941111     C* 11 ON  - VISUALIZZO IL VALORE MERCE E IL FLAG VALORE MERCE
064200941111     C     §1PVVM        COMP      'S'                                    11
064300941111     C* 28 ON  - VISUALIZZO IL TIPO APPLICAZIONE
064400941111     C     §1PVCA        COMP      'S'                                    28
064500941116     C* 38 ON  - VISUALIZZO IL MINIMO  TARIFFA
064600941116     C     §1PVMN        COMP      'S'                                    38
064700941116     C* 39 ON  - VISUALIZZO IL MASSIMO TARIFFA
064800941116     C     §1PVMX        COMP      'S'                                    39
064900980514     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO FITTIZIO
065000050531     C     VIDFTC        COMP      'C'                                    43
065100060802     C* 82 ON  - VISUALIZZO DATA riferimento prezzo base carburante
065200060802     C     §1pdrf        comp      'S'                                    82
065300060802      * Salvo il flag che mi permette di inserire una tariffa particolare
065400021209      * a qtà anche se la tariffa non è a qtà
065500021209     c                   Movel     §1pFqt        Wfqt
065600050531    4C                   ENDIF
065700941020     C*
065800941110     C                   EXSR      VIDEO2
065900050531    3c                   endif
066000130125
066100130125     c     nomantp       tag
066200920827     C*
066300940913     C     NRR           CHAIN     TA67S02                            31
066400920529     C                   MOVEL     ' '           VIDSCE
066500920708     C*
066600920619     C* 07 ON - PREMUTO  CMD6-AGGIORNAMENTO
066700920619     C   07              MOVEL     'S'           VIDFLG
066800990907     C   07              Z-ADD     UDATE8        VIDDUV
066900920708     C*
067000920618     C* 09 ON - PREMUTO  CMD16-ANNULLAMENTO
067100920708     C   09              MOVEL     ' '           VIDFLG
067200990907     C   09              CLEAR                   VIDDUV
067300050531     c* Se errore, mi posizione sulla tariffa particolare
067400050531     c                   if        *in13
067500050531     c                   seton                                        40
067600050531     c                   z-add     nrr           rec
067700050531     c                   endif
067800050531     c
067900940913     C                   UPDATE    TA67S02
068000050531    2C                   END
068100920708     C*
068200050531     c                   if        not *in13
068300940913     C                   READC     TA67S02                                31
068400050531     c                   else
068500050531     c                   seton                                        31
068600050531     c                   endif
068700050531     c
068800050531    1c                   ENDdo
068900921119     C                   GOTO      INIZIO
069000920529     C*
069100920529     C     FINE          TAG
069200921126     C                   MOVEL     PARAM5        KPJBU
069300120320
069400120320     c                   IF        *in97
069500120320     c                   eval      parm48 = tnsb48ds
069600120320     c                   ENDIF
069700120320
069800990917      * CHIUSURA PGM RICERCA BOLLE
069900990917     C                   CLEAR                   DSBS02
070000990917     C                   MOVEL     'C'           T02TLA
070100990917     C                   CALL      'TIBS02R'
070200990917     C                   PARM                    KPJBA
070300990917     C                   PARM                    DSBS02
070400921126     C*
070500920529     C                   SETON                                        LR
070600920609     C*
070700941110     C*--- CARICAMENTO TABELLE ---------------------------------------*
070800920609     C     CARTAB        BEGSR
070900941110     C****  CODICI TARIFFA E DESCRIZIONI  ****
071000920609     C                   MOVEL     'TR'          COD
071100920609     C                   Z-ADD     1             X                 3 0
071200980317     C* CARICO I TIPI TARIFFE CHE VENGONO GESTITE DALLA SINGOLA TARIFFA
071300980317     C* PARTICOLARE
071400990614    1C     WTT(X)        DOWNE     '  '
071500980317     C                   MOVEL     WTT(X)        KEY
071600980317     C     KTAB          CHAIN     TABEL                              30
071700980317    2C     *IN30         IFEQ      '0'
071800980317     C     TBLFLG        ANDEQ     *BLANKS
071900940921     C                   MOVEL     TBLUNI        DSTR
072000940913     C*
072100980317     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE PARTICOLARI
072200980317    3C     §TRUPA        IFEQ      'S'
072300920612     C                   MOVEL     TBLKEY        TTR(X)
072400941110     C                   MOVEL     §TRDES        DTR(X)
072500950112     C* IMPORTO TARIFFA  IN % O £ ("S"=TARIFFA IN % - " "=TARIFFA IN £)
072600941110     C                   MOVEL     §TRTAP        TTP(X)
072700950119     C* SCAGLIONE CON DECIMALI O SENZA ("S"=CON DECIM." "=SENZA DECIM.)
072800950112     C                   MOVEL     §TRSAP        TSP(X)
072900980429     C* ARROTONDAMENTI
073000980429     C                   MOVEL     §TRARR        ARR(X)
073100980429     C* RAPPORTO PESO VOLUME
073200980429     C                   MOVEL     §TRRPV        RPV(X)
073300980317    3C                   END
073400980316     C*
073500980317    2C                   END
073600980317     C*
073700980317     C                   ADD       1             X
073800980317    1C                   ENDDO
073900980317     C*
074000980317     C                   ENDSR
074100980317     C*-------------------------------------------------------------------------
074200980317     C     CARNAZ        BEGSR
074300950112     C****  CODICI NAZIONE  ****
074400950112     C                   Z-ADD     1             X
074500950112     C                   MOVEL     '15'          COD
074600950112     C     KTAB2         SETLL     TABEL
074700950112     C     KTAB2         READE     TABEL                                  32
074800950112     C*
074900950112     C     *IN32         DOWEQ     *OFF
075000950112     C     TBLFLG        IFEQ      ' '
075100950112     C                   MOVEL     TBLUNI        DS15
075200950112     C     §15ITA        IFEQ      'I'
075300950112     C                   MOVEL     TBLKEY        NAZ(X)
075400950112     C                   ADD       1             X
075500950112     C                   ENDIF
075600950112     C                   ENDIF
075700950112     C*
075800950112     C     KTAB2         READE     TABEL                                  32
075900950112     C                   ENDDO
076000050531     c*
076100950112     C*
076200920609     C                   ENDSR
076300080609
076400080609      *------------------------------------------------------------------------*
076500080609      * Carico filiali abilitate al lasciato avviso
076600080609      *------------------------------------------------------------------------*
076700080609     c     carlav        begsr
076800080609
076900080609     c                   clear                   sklav
077000080609     c                   clear                   xx
077100080609
077200080609     c/exec sql
077300080609     c+ declare lav cursor for select tntbe01l.*
077400080609     c+ from tntbe01l
077500080609     c+ where tbecod='LAV' and tbeatb = ''
077600080609     c/end-exec
077700080609
077800080609     c/exec sql
077900080609     c+ open lav
078000080609     c/end-exec
078100080609
078200080609     c                   do        *hival
078300080609
078400080609     c/exec sql
078500080609     c+ fetch next from lav into :tntbeds
078600080609     c/end-exec
078700080609
078800080609     c                   select
078900080609     c                   when      sqlcod = 100
079000080609     c                   leave
079100080609     c                   when      sqlcod < 0
079200080609     c                   other
079300080609     c                   eval      dlav = tbeuni
079400080609      * carico la schiera della filiali abilitate
079500080616     c                   if        d§lavtar > dateu8
079600080609     c                   iter
079700080609     c                   endif
079800080609     c                   eval      xx = xx + 1
079900080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
080000080609
080100080609     c                   endsl
080200080609
080300080609     c                   enddo
080400080609
080500080609     c/exec sql
080600080609     c+ close lav
080700080609     c/end-exec
080800080609
080900080609
081000080609     c                   endsr
081100920609     C*
081200920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
081300920529     C     CARICA        BEGSR
081400920529     C                   SETOFF                                       3031
081500920529     C* PULIZIA SUBFILE
081600920529     C                   EXSR      PULSF
081700920529     C*
081800920529     C                   Z-ADD     0             NRR               4 0
081900920529     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
082000920529     C                   MOVEL     '1P'          COD
082100920608     C     KTAB2         SETLL     TABEL
082200920608     C     KTAB2         READE     TABEL                                  30
082300970325    1C     *IN30         DOWEQ     '0'
082400990621     C* CONTROLLO VALIDITA' TARIFFA PARTICOLARE PER LA GESTIONE TARIFFE
082500970325    2C     TBLFLG        IFEQ      *BLANKS
082600920827     C                   MOVEL     TBLUNI        DS1P
082700020204      * FLAG DI COMODO PER TABELLA 'SP'
082800020204     C                   CLEAR                   FLAGSP
082900080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
083000080609      * per le filiali non abilitate, accetto cartello
083100051012     c                   If        tblkey = 'c       '  and
083200080609     c                             kscfil <> 888
083300080609     c                   if        %lookup(kscfil:sklav) = *zeros
083400080609     c                             and %lookup(999:sklav) = *zeros
083500051012     c                   goto      legtab
083600080609     c                   endif
083700051012     c                   endif
083800060303      * ESCLUDO la tariffa particolare "d" se tariffa di cartello
083900060303     c                   if        tblkey = 'd       '  and *in44
084000060303     c                   goto      legtab
084100060303     c                   endif
084200051012      *
084300020204      * Carico solo le tariffe particolari che hanno flag <> 'N'
084400020204      * Per tariffa con tipo servizio poste controllo flag §1pfpt
084500020201     C     PARTSP        IFEQ      'P'
084600020201     C     §1PFPT        ANDEQ     'N'
084700020201     C                   GOTO      LEGTAB
084800020201     C                   ENDIF
084900020201      * Per tariffa con Network DPD controllo flag §1pfdp
085000020201     C     FLGNET        IFEQ      'D'
085100020201     C     §1PFDP        ANDEQ     'N'
085200020201     C                   GOTO      LEGTAB
085300020201     C                   ENDIF
085400020201      * Per tariffa con Network FedEx controllo flag §1pffe
085500020201     C     FLGNET        IFEQ      'F'
085600020201     C     §1PFFE        ANDEQ     'N'
085700040428      * tariffa FEDEX abilitata solo per le tariffe di cartello e 44 spento
085800040428      * (44 acceso solo se sto modificando una tariffa di cartello)
085900040428     c     flgnet        oreq      'F'
086000040428     c     §1pffe        andeq     'C'
086100040428     c     *in44         andeq     *off
086200020201     C                   GOTO      LEGTAB
086300020201     C                   ENDIF
086400020201      * Per tariffa Italia controllo flag §1ptco
086500020201     C     PARFIE        IFEQ      'I'
086600020205     C     PARTSP        ANDNE     'P'
086700020201     C     §1PTCO        ANDEQ     'N'
086800020201     C                   GOTO      LEGTAB
086900020201     C                   ENDIF
087000020201      * Per tariffa Estero controllo flag §1pfie
087100020201     C     PARFIE        IFEQ      'E'
087200020205     C     FLGNET        ANDEQ     ' '
087300020201     C     §1PFIE        ANDEQ     'N'
087400020201     C                   GOTO      LEGTAB
087500020201     C                   ENDIF
087600020225      * Per tariffe di cartello verifico che non sia tipo servizio "P"
087700020225     C     PARFIE        IFEQ      ' '
087800020225     C     PARTSP        ANDNE     'P'
087900020225     C     §1PTCO        ANDEQ     'N'
088000020225     C                   GOTO      LEGTAB
088100020225     C                   ENDIF
088200020201      *
088300020201      * Utilizzabile in tariffa
088400000420     C**
088500970325    3C     §1PUTA        IFEQ      'S'
088600970325     C*
088700920529     C                   MOVEL     TBLKEY        VIDFTC
088800920529     C                   MOVEL     §1PDES        VIDDES
088900050531     C                   MOVEL     §1Peli        VIheli
089000020201      *
089100020201      * Per tariffa FedEx controllo se devo recupare la descrizione
089200020201      * dalla tabella SP
089300020201     C     FLGNET        IFEQ      'F'
089400020201     C     §1PDFE        ANDEQ     'S'
089500020204     C                   MOVEL     '1'           FLAGSP            1
089600020201     C                   CLEAR                   DSSP
089700020201     C                   MOVEL     'SP'          COD
089800020201     C                   MOVEL     *BLANKS       KEY
089900020201     C                   MOVEL     VIDFTC        KEY
090000020204     C     KTAB          CHAIN     TABEL                              34
090100020201     C     *IN34         IFEQ      *OFF
090200020201     C                   MOVEL     TBLUNI        DSSP
090300020201     C                   ENDIF
090400020201     C     §SPDFE        IFNE      *BLANKS
090500020201     C                   MOVEL     §SPDFE        VIDDES
090600020201     C                   ENDIF
090700020201     C                   ENDIF
090800020201      *
090900920529     C* CAMPO SELEZIONE
091000920529     C                   MOVEL     ' '           VIDSCE
091100920529     C*
091200061011     C  n12KTPT          CHAIN     TITPT000                           31
091300061011     c   12ktpt          chain     tiopt000                           31
091400990903     C                   Z-ADD     0             VIDDUV
091500970325    5C     *IN31         IFEQ      '0'
091600920529     C* CONTROLLO VALIDITA' TARIFFA
091700970325    6C     TPTATB        IFEQ      ' '
091800920529     C* TARIFFA PARTICOLARE GIA' ESISTENTE
091900920529     C                   MOVEL     'S'           VIDFLG
092000990903     C* VALORIZZO L'ULTIMA DATA VARIAZIONE
092100990903     C*
092200990903     C* GIRO DATA
092300990903     C*
092400060802     C                   Z-ADD     TPTDUV        G08INV
092500060802     C                   MOVEL     '3'           G08ERR
092600990903     C                   CALL      'XSRDA8'
092700990903     C                   PARM                    WLBDAT
092800990903     C*
092900060802     C                   Z-ADD     G08DAT        VIDDUV
093000990903     C*
093100920529     C                   ELSE
093200920529     C* TARIFFA ANNULLATA
093300920708     C                   MOVEL     ' '           VIDFLG
093400970325    6C                   END
093500920529     C*
093600970325   X5C                   ELSE
093700920529     C* TARIFFA PARTICOLARE NON ESISTENTE
093800920708     C                   MOVEL     ' '           VIDFLG
093900970325    5C                   END
094000920529     C*
094100990217     C* SE C'E' LA TARIFFA -->VISUALIZZO
094200130607    5C**** VIDFLG        IFEQ      'S'
094300990217     C* SE NON E' UNA TARIFFA CHE RICHIEDE L'ABILITAZIONE --> VISUAL.
094400990217     C*
094500920529     C                   ADD       1             NRR
094600940913     C                   WRITE     TA67S02
094700130607    5C*****              END
094800010920    3C                   END
094900010920    2C                   END
095000000420     C*
095100000420     C     LEGTAB        TAG
095200020204     C     FLAGSP        IFNE      *BLANKS
095300020201     C                   MOVEL     '1P'          COD
095400020204     C     KTAB          SETGT     TABEL
095500020204     C                   ENDIF
095600920611     C     KTAB2         READE     TABEL                                  30
095700970325    1C                   END
095800920529     C                   Z-ADD     1             REC
095900990621     C*
096000920529     C                   ENDSR
096100920529     C*
096200920529     C*--- PULIZIA SUBFILE -------------------------------------------*
096300920529     C     PULSF         BEGSR
096400920529     C                   SETON                                        35
096500940913     C                   WRITE     TA67C02
096600920529     C                   SETOFF                                       35
096700920529     C                   ENDSR
096800000829     C*---------------------------------------------------------------*
096900000829     C*    CARICO DATI IN SUBFILE COPIA TARIFFE DA CARTELLO           *
097000000829     C*---------------------------------------------------------------*
097100000829     C     COPCAR        BEGSR
097200000829     C                   SETOFF                                       8081
097300000829     C* PULIZIA SUBFILE
097400091104     c                   z-add     1             rec
097500000829     C                   SETON                                        80
097600000829     C                   WRITE     TA67C09
097700000829     C                   SETOFF                                       80
097800000829     C*
097900000829     C                   Z-ADD     0             NR9               4 0
098000000829     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
098100000829     C                   MOVEL     '1P'          COD
098200000829     C     KTAB2         SETLL     TABEL
098300000829     C     KTAB2         READE     TABEL                                  30
098400000829    1C     *IN30         DOWEQ     '0'
098500000829     C* CONTROLLO VALIDITA' TARIFFA PARTICOLARE PER LA GESTIONE TARIFFE
098600000829    2C     TBLFLG        IFEQ      *BLANKS
098700000829     C                   MOVEL     TBLUNI        DS1P
098800020204      * FLAG DI COMODO PER TABELLA 'SP'
098900020204     C                   CLEAR                   FLAGSP
099000080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
099100080609      * per le filiali non abilitate, accetto cartello
099200051012     c                   If        tblkey = 'c       '  and
099300080609     c                             kscfil <> 888
099400080609     c                   if        %lookup(kscfil:sklav) = *zeros
099500080609     c                             and %lookup(999:sklav) = *zeros
099600051012     c                   goto      doptab
099700080609     c                   endif
099800051012     c                   endif
099900130125       //?Se non posso manutenzionare le tariffe non carico le tariffe
100000130125       //?particolari NON sbloccate
100100130125     c                   IF        wBlocca and §1Pmodtp = *blanks
100200130125     c                   goto      doptab
100300130125     c                   ENDIF
100400051012      *
100500020204      * Carico solo le tariffe particolari che hanno flag = 'O'
100600020201      * Per tariffa con tipo servizio poste controllo flag §1pfpt
100700020201     C     PARTSP        IFEQ      'P'
100800020201     C     §1PFPT        ANDNE     'O'
100900020201     C                   GOTO      DOPTAB
101000020201     C                   ENDIF
101100020201      * Per tariffa con Network DPD controllo flag §1pfdp
101200020201     C     FLGNET        IFEQ      'D'
101300020201     C     §1PFDP        ANDNE     'O'
101400020201     C                   GOTO      DOPTAB
101500020201     C                   ENDIF
101600020201      * Per tariffa con Network FedEx controllo flag §1pffe
101700020201     C     FLGNET        IFEQ      'F'
101800020201     C     §1PFFE        ANDNE     'O'
101900020201     C                   GOTO      DOPTAB
102000020201     C                   ENDIF
102100020201      * Per tariffa Italia controllo flag §1ptco
102200020201     C     PARFIE        IFEQ      'I'
102300020205     C     PARTSP        ANDNE     'P'
102400020201     C     §1PTCO        ANDNE     'O'
102500020201     C                   GOTO      DOPTAB
102600020201     C                   ENDIF
102700020201      * Per tariffa Estero controllo flag §1pfie
102800020201     C     PARFIE        IFEQ      'E'
102900020205     C     FLGNET        ANDEQ     ' '
103000020201     C     §1PFIE        ANDNE     'O'
103100020201     C                   GOTO      DOPTAB
103200020201     C                   ENDIF
103300020201      *
103400020201      * Utilizzabile in tariffa
103500000829     C**
103600000829    3C     §1PUTA        IFEQ      'S'
103700000829     C*
103800000829     C*
103900000918     C                   MOVEL     TBLKEY        V09FTC
104000061011     C  n12KTPT9         CHAIN     TITPT000                           31
104100061011     c   12ktpt9         chain     tiopt000                           31
104200000829    4C     *IN31         IFEQ      '0'
104300000829     C* CONTROLLO VALIDITA' TARIFFA
104400000829     C     TPTATB        ANDEQ     ' '
104500000829     C* TARIFFA PARTICOLARE GIA' ESISTENTE
104600000829     C                   GOTO      DOPTAB
104700000829     C*
104800000829    4C                   ENDIF
104900000829     C* TARIFFA ANNULLATA   OPPURE
105000000829     C* TARIFFA PARTICOLARE NON ESISTENTE
105100000829     C*                     SCRIVO IL RECORD NEL SUBFILE
105200000829     C                   MOVEL     §1PDES        V09DES
105300020201      *
105400020201      * Per tariffa FedEx controllo se devo recupare la descrizione
105500020201      * dalla tabella SP
105600020201     C     FLGNET        IFEQ      'F'
105700020201     C     §1PDFE        ANDEQ     'S'
105800020204     C                   MOVEL     '1'           FLAGSP            1
105900020201     C                   CLEAR                   DSSP
106000020201     C                   MOVEL     'SP'          COD
106100020201     C                   MOVEL     *BLANKS       KEY
106200020201     C                   MOVEL     V09FTC        KEY
106300020204     C     KTAB          CHAIN     TABEL                              34
106400020201     C     *IN34         IFEQ      *OFF
106500020201     C                   MOVEL     TBLUNI        DSSP
106600020201     C                   ENDIF
106700020201     C     §SPDFE        IFNE      *BLANKS
106800020201     C                   MOVEL     §SPDFE        V09DES
106900020201     C                   ENDIF
107000020201     C                   ENDIF
107100020201      *
107200000829     C* CAMPO SELEZIONE
107300000829     C                   MOVEL     ' '           V09SCE
107400000829     C                   ADD       1             NR9
107500000829     C                   WRITE     TA67S09
107600000829    3C                   END
107700000829    2C                   END
107800000829     C*
107900000829     C     DOPTAB        TAG
108000020204     C     FLAGSP        IFNE      *BLANKS
108100020201     C                   MOVEL     '1P'          COD
108200020204     C     KTAB          SETGT     TABEL
108300020204     C                   ENDIF
108400000829     C     KTAB2         READE     TABEL                                  30
108500000829    1C                   END
108600000829     C*
108700000829     C* GESTIONE DEL SUBFILE DELLA COPIA TARIFFE
108800000915     C                   SETON                                        81
108900000918     C     NR9           COMP      0                                      80
109000000829     C                   Z-ADD     1             NR9
109100000829     C                   WRITE     TA67T01
109200000829     C                   WRITE     TA67Z09
109300000829     C                   EXFMT     TA67C09
109400000829     C*
109500000829    1C     *INKL         IFNE      *ON
109600070521     c     *in80         andeq     *off
109700000829     C* LEGGO IL SUBFILE
109800000901    2C                   DO        *HIVAL
109900000829     C                   READC     TA67S09                                30
110000000915    3C     *IN30         IFEQ      *ON
110100000829     C                   LEAVE
110200000901    3C                   ENDIF
110300000901    3C     V09SCE        IFEQ      '1'
110400061011     C* CARICO LA DS PER PGM DI COPIA TARIFFA
110500061011     c                   clear                   tnta25ds
110600061011     c  n12              eval      ta25tipo = 'T'
110700061011     c   12              eval      ta25tipo = 'O'
110800061011     c                   eval      ta25ksco = vidcli
110900061011     c                   move      vidctr        ta25ctro
111000061011     c                   move      vidprg        ta25prgo
111100061011     c  n12              eval      ta25tipn = 'T'
111200061011     c   12              eval      ta25tipn = 'O'
111300061011     c                   eval      ta25kscn = vidcli
111400061011     c                   move      vidctr        ta25ctrn
111500061011     c                   move      vidprg        ta25prgn
111600061011     c                   eval      ta25tam = 'N'
111700061011     c                   eval      ta25fie = parfie
111800061011     c                   eval      ta25tam = 'N'
111900061011     c                   eval      ta25tad = 'N'
112000061011     c                   eval      ta25tpt = 'N'
112100061011     c                   eval      ta25tpd = 'N'
112200061011     c                   eval      ta25tgc = 'N'
112300061011     c                   eval      ta25cat = 'N'
112400061011     c                   eval      ta25inglo = 'S'
112500061011
112600061011     c                   eval      ta25ftcr = v09ftc
112700061011
112800061011     c                   call      'TNTA25R'
112900061011     c                   parm                    kpjba
113000061011     c                   parm                    tnta25ds
113100061011     c                   parm                    tnta25tad
113200061011     c                   parm                    tnta25tgc
113300061011     c                   parm                    tnta25tpd
113400061011     c                   parm                    tnta25tpt
113500000829     C*
113600000901    3C                   ENDIF
113700000901    2C                   ENDDO
113800000829     C*
113900000901    1C                   ENDIF
114000000829     C*
114100000829     C                   ENDSR
114200920529     C*
114300920529     C*--- SR SECONDA VIDEATA ----------------------------------------*
114400920529     C     VIDEO2        BEGSR
114500920619     C                   SETOFF                                       010207
114600920619     C                   SETOFF                                       090304
114700941116     C                   SETOFF                                       062425
114800060303
114900060303     c                   eval      *in78 = *off
115000060303
115100920529     C*
115200941116     C* 24 ON - TIPO CONSEGNA = "C - ASSICURAZIONE X CONTO"
115300990217     C     §1PASC        IFEQ      'S'
115400990217     C                   SETON                                            24
115500990217     C                   ENDIF
115600941116     C* 25 ON - TIPO CONSEGNA = "R - RCV"
115700990217     C     §1PRCV        IFEQ      'S'
115800990217     C                   SETON                                            25
115900990217     C                   ENDIF
116000941110     C*
116100920609     C* TARIFFA PARTICOLARE NON ESISTENTE
116200920708     C     VIDFLG        IFEQ      ' '
116300920529     C                   SETON                                        01
116400920529     C                   ELSE
116500920609     C* TARIFFA PARTICOLARE GIA' ESISTENTE
116600920529     C                   SETON                                        02
116700920529     C                   END
116800920529     C*
116900921006     C* PULIZIA CAMPI DEL FORMATO E DEL SFLCONTROL
117000921006     C                   MOVEL     ' '           VI2TPG
117100920529     C                   EXSR      PULCTL
117200921126     C*
117300921005     C* CARICO TIPO TARIFFA
117400061011     C   02
117500061011     Cann12KTPT          CHAIN     TITPT000                           33
117600061011     c   02
117700061011     can 12ktpt          chain     tiopt000                           33
117800921005     C   02
117900921005     CANN33              MOVEL     TPTTPG        VI2TPG
118000921005     C   02
118100921005     CANN33              MOVEL     TPTTPG        SAVTPG
118200921118     C   01              MOVEL     ' '           SAVTPG
118300060303
118400060303      * se tariffa particolare "d"
118500060303      * i dati vanno protetti
118600060303     c                   if        vidftc = 'd '
118700060303     c                   eval      *in78 = *on
118800060303      * se non è presente carico i dati di dft
118900060303     c                   if        *in01
119000060303     c                   eval      vi2tpg = '3'
119100060303     c                   endif
119200060303     c                   endif
119300921005     C*
119400921005     C     FOR05         TAG
119500921005     C                   SETOFF                                       22
119600960520     C*
119700940913     C                   WRITE     TA67T01
119800940913     C                   WRITE     TA67Z08
119900921005     C     FOR06         TAG
120000941110     C                   EXFMT     TA67D06
120100921005     C*
120200921005     C** CMD12 - FINE LAVORO
120300921005     C   KL              GOTO      FINE2
120400921005     C*
120500921005     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
120600921005     C   KQ              EXSR      ANNTAR
120700921005     C   KQ              GOTO      FINE2
120800921005     C*
120900921005     C* CONTROLLI FORMATO
121000921005     C                   EXSR      CONTR2
121100921005     C   98              GOTO      FOR05
121200921005     C*
121300921005     C   99              GOTO      FOR06
121400921005     C*
121500921005     C     INVID2        TAG
121600921007     C                   Z-ADD     0             NR2
121700921007     C                   Z-ADD     0             SAVNR2
121800921005     C                   SETON                                        22
121900921005     C* CARICO DATI DEL SFLCONTROL
122000921215     C   02
122100921215     CANN29              MOVEL     TPTDS         COMTPT
122200921006     C   02
122300921006     CANN29              EXSR      CARCTL
122400060303
122500060303      * se tariffa particolare "d" non presente carico i dati di dft
122600060303     c                   if        *in01 and vidftc = 'd '
122700060303      * valore merce 6,2
122800060303     c                   z-add     6,2           vi2vlm
122900060303     c                   z-add     vi2vlm        savvlm
123000060303      * tipo valore '3' già impostato in precedenza se immissione
123100060303     c                   endif
123200060802      * se tariffa particolare "f" non presente carico i dati di dft
123300060802     c                   if        *in01 and vidftc = 'f '
123400060802     c                   seton                                        86
123500061207      *? ATTENZIONE CHIODO!!!  ?
123600061207      * se data del giorno è inferiore a 31/12/2006
123700061207      * chiodo fisso 31/12/2006
123800061207      * altrimenti metto la data del giorno
123900140219     c****               if        dateu8 < 20061231
124000140219     c****               eval      vi2dpb = 31122006
124100140219     c****               else
124200140219     c****               z-add     udate8        vi2dpb
124300140219     c****               endif
124400131216      * verifico se esiste il file del prezzo base da impostare in base alla
124500131216      * data validità
124600131217     c     dateu8        setll     tidpb01l
124700131216     c                   read      tidpb01l
124800131216     c                   if        %found(tidpb01l)
124900150217     c                   z-add     dpbdpb        vo2dpb
125000140115     c                   eval      VI2pma = DPBpma
125100131216     c                   endif
125200131216      * recupero il prezzo del gasolio corrispondente
125300131218     c                   eval      Kpmg = dpbdpb
125400131218     c                   exsr      Rec_pmg
125500131216
125600131218     c                   endif
125700060303      *
125800920611     C* PULIZIA SUBFILE
125900920611     C                   EXSR      PULSF2
126000941110     C*
126100970520     C* 06 ON - APPLICAZIONE CON INOLTRO  PER ISOLE/LOC.DISAGIATE/C.STO
126200970520     C     §1PAIN        COMP      'S'                                    06
126300941110     C*
126400920529     C* CARICO SUBFILE
126500921214     C   02              EXSR      CARSFL
126600920609     C* SCRIVO RECORD DI NUOVA IMMISSIONE
126700921005     C   01              EXSR      SCRIVI
126800920617     C*
126900920617     C                   Z-ADD     1             REC2
127000920611     C     FOR01         TAG
127100940913     C                   WRITE     TA67T01
127200940913     C                   WRITE     TA67Z05
127300941110     C                   WRITE     TA67D06
127400920529     C     FORCT         TAG
127500941110     C                   EXFMT     TA67C07
127600950119     C*
127700950119     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN13)
127800950119     C                   CLEAR                   VI2MSG
127900950119     C                   SETOFF                                       1374
128000140131     c                   setoff                                       62
128100920609     C*
128200111122     C** CMD12 - FINE LAVORO
128300111122     c                   If        *Inkl  and *in62
128400111122     C                   SETON                                        99
128500111122     c                   goto      forct
128600111122     C                   ENDIF
128700111122     c
128800920611     C** CMD12 - FINE LAVORO
128900050704     c                   If        *Inkl
129000111122      /free
129100111122       // Non posso fare F12 se ho annullato tutte le righe di dettaglio
129200111130        IF not *in01;
129300111122         *in62 = *on;
129400111123         nr2 = 1;
129500111123         chain nr2 TA67S07;
129600111123         DOW %found and *in62;
129700111130           IF  VI2cts <> *blanks;
129800111123             *in62 = *off;
129900111123           ENDIF;
130000111123           update TA67S07;
130100111122           nr2 += 1;
130200111122           chain nr2 TA67S07;
130300111122         ENDDO;
130400111123      /end-free
130500111123     c                   IF        *in62
130600111123     c                   eval      *in99 = *in62
130700111123     c                   goto      forct
130800111123     c                   ENDIF
130900111130      /free
131000111130          *in62 = *on;
131100111130          If not *in12 ;
131200111130          setll (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
131300111130          reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
131400111130          DOW  not %eof(TITPD01L);
131500111130            IF  TPDatb = *blanks;
131600111130              *IN62 = *off;
131700111130            ENDIF;
131800111130            reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
131900111130          ENDDO;
132000111130          Endif ;
132100111130          If *in12 ;
132200111130            setll (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
132300111130            reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
132400111130            DOW  not %eof(TIOPD01L);
132500111130              IF  TPDatb = *blanks;
132600111130               *IN62 = *off;
132700111130             ENDIF;
132800111130             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
132900111130           ENDDO;
133000111130           Endif ;
133100111130      /end-free
133200111130     c                   IF        *in62
133300111130     c                   eval      *in99 = *in62
133400111130     c                   goto      forct
133500111130     c                   ENDIF
133600111130     c                   ENDIF
133700111122     c
133800050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
133900050704      * per codice di tassazione
134000050704     c                   Clear                   savcts
134100050704     c                   If        vi2tpg = '2'
134200061011     c  n12ktpt          Setll     titpd01l
134300061011     c   12ktpt          Setll     tiopd01l
134400050704     c                   Do        *Hival
134500061011     c  n12ktpt          Reade(n)  titpd01l
134600061011     c   12ktpt          Reade(n)  tiopd01l
134700061011     c                   If        %eof
134800050704     c                   Leave
134900050704     c                   EndIf
135000050704     c                   If        tpdatb <> *Blanks
135100050704     c                   Iter
135200050704     c                   EndIf
135300050704     c                   If        tpdcts = savcts
135400050704     c                   Eval      *in69 = *on
135500050704     c                   Eval      *in13 = *on
135600050704     c                   Eval      vi2msg = msg(6)
135700050704     c                   Else
135800050704     c                   Eval      savcts = tpdcts
135900050704     c                   EndIf
136000050704     c                   EndDo
136100050704     c                   If        *In13
136200050704     c                   Goto      invid2
136300050704     c                   EndIf
136400050704     c                   EndIf
136500050704     C                   GOTO      FOR05
136600050704     c                   EndIf
136700920611     C*
136800920611     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
136900920611     C   KQ              EXSR      ANNTAR
137000920611     C   KQ              GOTO      FINE2
137100140220     C** CMD04 - Interrogazione prezzi del gasolio
137200140220     C   KD              EXSR      RecPrezzi
137300140220     C   KD              GOTO      Forct
137400920611     C*
137500920611     C** CMD9  - AGGIUNTA RIGHE
137600920611     C   KI
137700920529     C* SCORRIMENTO
137800920611     COR 08              DO
137900920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
138000920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
138100920619     C                   SETON                                        0336
138200920611     C                   EXSR      SCRIVI
138300920529     C                   GOTO      FORCT
138400920529     C                   END
138500920529     C*
138600920611     C* CONTROLLO DATI SUBFILE CONTROL
138700920611     C                   EXSR      CONCTL
138800941111     C   98              GOTO      FOR01
138900941111     C*
139000920619     C   99              GOTO      FORCT
139100920529     C*
139200921007     C* CONTROLLO DATI SUBFILE
139300921007     C                   EXSR      CONSFL
139400921007     C   99              GOTO      FORCT
139500140131
139600921007     C*
139700920623     C** ENTER - REGISTRA
139800920624     C* REGISTRO CAMPI DEL SUBFILE
139900920709     C                   EXSR      REGIS
140000920624     C* REGISTRO CAMPI DEL SUBFILE CONTROL
140100920709     C                   EXSR      REGIS2
140200920623     C*
140300921007     C* SPENGO L' *IN29  PERCHE' IN QUESTO MODO RIESCO A POSIZIONARE
140400921007     C*   IL CURSORE SULLA PRIMA RIGA DI DETTAGLIO  - (ALTRIMENTI
140500921007     C*   RESTAVA POSIZIONATO SUL PRIMO CAMPO DEL SFLCONTROL) -
140600921007     C                   SETOFF                                       29
140700111123      /free
140800111123       // Se ho dato F6 o F14 verifico di avere almeno 1 rcd di dettaglio
140900111123         IF  *inkf or *inkn;
141000111123           *in62 = *on;
141100111130           If not *in12 ;
141200111123           setll (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
141300111123           reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
141400111123           DOW  not %eof(TITPD01L);
141500111123             IF  TPDatb = *blanks;
141600111123               *IN62 = *off;
141700111123             ENDIF;
141800111123             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
141900111123           ENDDO;
142000111130           Endif ;
142100111130           If *in12 ;
142200111130           setll (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
142300111130           reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
142400111130           DOW  not %eof(TIOPD01L);
142500111130             IF  TPDatb = *blanks;
142600111130               *IN62 = *off;
142700111130             ENDIF;
142800111130             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
142900111130           ENDDO;
143000111130           Endif ;
143100111123         ENDIF;
143200140131      /end-free
143300140131     c                   IF        *in62
143400140131     c                   eval      *in99 = *in62
143500140131     c                   goto      invid2
143600140131     c                   ENDIF
143700050704
143800050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
143900050704      * per codice di tassazione
144000050704     c                   Clear                   savcts
144100050704     c                   If        vi2tpg = '2'
144200061011     c  n12ktpt          Setll     titpd01l
144300061011     c   12ktpt          Setll     tiopd01l
144400050704     c                   Do        *Hival
144500061011     c  n12ktpt          Reade(n)  titpd01l
144600061011     c   12ktpt          Reade(n)  tiopd01l
144700061011     c                   If        %eof
144800050704     c                   Leave
144900050704     c                   EndIf
145000050704     c                   If        tpdatb <> *Blanks
145100050704     c                   Iter
145200050704     c                   EndIf
145300050704     c                   If        tpdcts = savcts
145400050704     c                   Eval      *in69 = *on
145500050704     c                   Eval      *in13 = *on
145600050704     c                   Eval      vi2msg = msg(6)
145700050704     c                   Else
145800050704     c                   Eval      savcts = tpdcts
145900050704     c                   EndIf
146000050704     c                   EndDo
146100050704     c                   If        *In13
146200050704     c                   Goto      invid2
146300050704     c                   EndIf
146400050704     c                   EndIf
146500050704
146600921007     C*
146700920623     C** CMD6  - REGISTRA ED ESCE
146800921202     C  NKF
146900921202     CANNKN              GOTO      INVID2
147000920529     C*
147100920708     C     FINE2         ENDSR
147200920609     C*
147300920609     C*--- PULIZIA SUBFILE CONTROL -----------------------------------*
147400920609     C     PULCTL        BEGSR
147500930223     C                   MOVEL     ' '           UNO               1
147600920609     C                   Z-ADD     0             VI2ARL
147700940712     C                   Z-ADD     0             VI2ARF
147800920609     C                   Z-ADD     0             VI2ARO
147900930217     C                   Z-ADD     0             VI2RPV
148000980422     C                   MOVE      ' '           VI2APL
148100980428     C                   MOVE      ' '           VI2MAS
148200150219     c                   clear                   VO2vma
148300140115     c                   clear                   VI2pma
148400150217     C                   CLEAR                   VO2dpb
148500140219     C                   CLEAR                   VO2sca
148600140219     C                   CLEAR                   Vi2pmg
148700941111     C                   CLEAR                   TPG1
148800941111     C                   CLEAR                   DPG1
148900941111     C                   CLEAR                   TPG2
149000941111     C                   CLEAR                   DPG2
149100941111     C                   CLEAR                   TPG3
149200941111     C                   CLEAR                   DPG3
149300941111     C                   CLEAR                   TPG4
149400941111     C                   CLEAR                   DPG4
149500941111     C                   CLEAR                   TPG5
149600941111     C                   CLEAR                   DPG5
149700941111     C                   CLEAR                   TPG6
149800941111     C                   CLEAR                   DPG6
149900980508     C                   CLEAR                   TPG7
150000980508     C                   CLEAR                   DPG7
150100980508     C                   CLEAR                   TPG8
150200980508     C                   CLEAR                   DPG8
150300941111     C*
150400941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
150500941111    1C     *IN11         IFEQ      '1'
150600930217     C   24              Z-ADD     0             VI2VLM
150700930217     C*
150800930217     C* IN IMMISSIONE PER IL TIPO CONSEGNA "R", NEL CAMPO LIMITE
150900941111     C*    RISARCIBILE PROPONGO 12000 PRENDENDOLO DALLA TABELLA "1G"
151000990907     C   25              Z-ADD     §GELRP        VI2VLM
151100930217     C*
151200930223     C* IN IMMISSIONE PROPONGO IL CAMPO TIPO VALORE = 3-PESO KG.
151300930223     C                   MOVEL     '3'           VI2FVM
151400941111     C* DECODIFICA TIPO VALORE MERCE
151500930223     C                   MOVEL     *BLANKS       DESFVM
151600980316     C* DECODIFICA DALLA TABELLA TR
151700980316     C                   MOVEL     'TR'          COD
151800930223     C                   MOVEL     *BLANKS       KEY
151900930223     C                   MOVEL     VI2FVM        KEY
152000930223     C     KTAB          CHAIN     TABEL                              34
152100941111    2C     *IN34         IFEQ      '0'
152200980316     C                   MOVEL     TBLUNI        DSTR
152300980316     C* DECODIFICA DALLA TABELLA TR
152400980316     C                   MOVEL     §TRDES        DESFVM
152500941111    2C                   END
152600941111    1C                   END
152700941110     C*
152800980317     C* CARICO I TIPI TARIFFA
152900941110     C     TTR(1)        IFNE      ' '
153000941110     C                   MOVEL     TTR(1)        TPG1
153100941110     C                   MOVEL     DTR(1)        DPG1
153200941110     C                   END
153300941110     C     TTR(2)        IFNE      ' '
153400941110     C                   MOVEL     TTR(2)        TPG2
153500941110     C                   MOVEL     DTR(2)        DPG2
153600941110     C                   END
153700941110     C     TTR(3)        IFNE      ' '
153800941110     C                   MOVEL     TTR(3)        TPG3
153900941110     C                   MOVEL     DTR(3)        DPG3
154000941110     C                   END
154100941110     C     TTR(4)        IFNE      ' '
154200941110     C                   MOVEL     TTR(4)        TPG4
154300941110     C                   MOVEL     DTR(4)        DPG4
154400941110     C                   END
154500941110     C     TTR(5)        IFNE      ' '
154600941110     C                   MOVEL     TTR(5)        TPG5
154700941110     C                   MOVEL     DTR(5)        DPG5
154800941110     C                   END
154900941110     C     TTR(6)        IFNE      ' '
155000941110     C                   MOVEL     TTR(6)        TPG6
155100941110     C                   MOVEL     DTR(6)        DPG6
155200941110     C                   END
155300980428     C     TTR(7)        IFNE      ' '
155400980428     C                   MOVEL     TTR(7)        TPG7
155500980428     C                   MOVEL     DTR(7)        DPG7
155600980428     C                   END
155700980428     C     TTR(8)        IFNE      ' '
155800980428     C                   MOVEL     TTR(8)        TPG8
155900980428     C                   MOVEL     DTR(8)        DPG8
156000980428     C                   END
156100941110     C*
156200941111     C                   MOVEL     *BLANKS       COMTPT           38
156300920828     C                   ENDSR
156400140220     C*
156500140220     C*--- Recupero Prezzi del Gasolio -------------------------------*
156600140220     C     RecPrezzi     BEGSR
156700140220
156800140220     c                   clear                   tnta39ds
156900140220     c                   call      'TNTA39R'
157000140220     c                   parm                    kpjba
157100140220     c                   parm                    tnta39ds
157200140220
157300140220     c                   If        ota39f03 <> '1'
157400140220     c                   eval      vi2pmg = ota39pmg
157500150217     c                   eval      vo2dpb = ota39dtd
157600140220     c                   eval      vo2sca = ota39sca
157700140220     c                   Endif
157800140220
157900140220     C                   ENDSR
158000921005     C*
158100941116     C*--- CONTROLLO DATI FORMATO ------------------------------------*
158200921005     C     CONTR2        BEGSR
158300921006     C                   SETOFF                                       262729
158400941110     C                   SETOFF                                       989921
158500060802     C                   SETOFF                                       14
158600950119     C                   CLEAR                   WSCD
158700921005     C*
158800921005     C* TIPO TARIFFA OBBLIGATORIO
158900941116    1C     VI2TPG        IFEQ      '?'
159000941110     C                   MOVEL     'TR'          COD
159100941110     C*
159200921005     C                   MOVE      ' '           VI2TPG
159300001020     C                   MOVE      *BLANKS       KEY
159400921005     C                   MOVEL     *BLANKS       DES              25
159500921005     C                   CALL      'X§TABER'
159600921005     C                   PARM                    CODUT
159700921005     C                   PARM                    COD
159800921005     C                   PARM                    KEY
159900921005     C                   PARM                    DES
160000921005     C                   MOVEL     KEY           VI2TPG
160100941110     C                   SETON                                        98
160200921005     C                   GOTO      END2
160300941116    1C                   END
160400921005     C*
160500941116    1C     VI2TPG        IFEQ      ' '
160600921005     C                   SETON                                        4099
160700941116    1C                   END
160800941116     C*
160900921005     C* CONTROLLO SE ESISTE TIPO TARIFFA
161000941020     C                   Z-ADD     1             X
161100941020     C     VI2TPG        LOOKUP    TTR(X)                                 20
161200941110     C* CONTROLLO SE TARIFFA IN % O IN £
161300941110     C   20TTP(X)        COMP      'S'                                    14
161400950119     C* WSCD = "S" ---> SCAGLIONE CON DECIMALI
161500950119     C   20              MOVEL     TSP(X)        WSCD              1
161600980429     C* ARROTONDAMENTI
161700980429     C   20              MOVEL     ARR(X)        WARR              1
161800980429     C* RAPPORTO PESO VOLUME
161900980429     C   20              MOVEL     RPV(X)        WRPV              1
162000941110     C*
162100941110     C*
162200921005     C  N20              SETON                                        4199
162300921005     C   99              GOTO      END2
162400941020     C*
162500950119     C* I TIPI TARIFFA "4" E "5" SI POSSONO ACCETTARE SOLO SE LA TARIFFA
162600950119     C*   CLIENTE HA COME COD.TARIFFA RISPETTIVAMENTE "4xx" E "5xx"
162700021209      * se in tabella '1P' il flag §1PFQT è a Blank
162800021209     c                   If        Wfqt = *Blanks
162900950119     C                   MOVEL     VIDCTR        WCTR              1
163000950119    1C     VI2TPG        IFEQ      '4'
163100950119     C     WCTR          ANDNE     '4'
163200950119     C                   SETON                                        7099
163300950119     C                   GOTO      END2
163400950119    1C                   ENDIF
163500950119    1C     VI2TPG        IFEQ      '5'
163600950119     C     WCTR          ANDNE     '5'
163700950119     C                   SETON                                        7399
163800950119     C                   GOTO      END2
163900950119    1C                   ENDIF
164000021209     c                   EndIf
164100950119     C*
164200950112     C* IN VARIAZIONE: SE TARIFFA A % NON POSSO VARIARE IL TIPO TARIFFA
164300950119    1C   02SAVTPG        IFNE      VI2TPG
164400950119     C                   CLEAR                   WCOM
164500950119     C                   Z-ADD     1             X
164600950119     C*
164700950119     C     SAVTPG        LOOKUP    TTR(X)                                 20
164800950119     C* CONTROLLO SE TARIFFA IN % O IN £
164900950119     C   20TTP(X)        IFEQ      'S'
165000950119     C                   MOVEL     'P'           WCOM              1
165100950119     C                   ENDIF
165200950119     C*
165300950119    2C     *IN14         IFEQ      *ON
165400950119     C     WCOM          ANDEQ     ' '
165500950119     C     *IN14         OREQ      *OFF
165600950119     C     WCOM          ANDEQ     'P'
165700941116     C                   SETON                                        5199
165800941116     C                   GOTO      END2
165900950112    2C                   ENDIF
166000950112    1C                   ENDIF
166100941110     C*
166200921016     C* OGNI VOLTA CHE CAMBIO IL TIPO TARIFFA DEVO AZZERARE TUTTI
166300921016     C*   I CAMPI DEL SFLCONTROL
166400941116    1C     VI2TPG        IFNE      SAVTPG
166500921006     C                   SETON                                        29
166600921005     C                   EXSR      PULCTL
166700941116    1C                   END
166800921005     C*
166900921005     C* CONDIZIONO L'EMISSIONE DEGLI ARROTOND. E DEL RAPPORTO PESO/VOL.
167000980429     C     WARR          IFEQ      'S'
167100980429     C                   SETON                                        26
167200980429     C                   END
167300980429     C*
167400980429     C     WRPV          IFEQ      'S'
167500980429     C                   SETON                                        27
167600980429     C                   END
167700941110     C*
167800941110     C* 21 ON - SE TIPO TARIFFA = 0  PROTEGGO IL CAMPO TIPO VALORE
167900981027     C   25              SETON                                        21
168000921005     C*
168100921005     C     END2          ENDSR
168200920529     C*
168300920611     C*--- PULIZIA SUBFILE -------------------------------------------*
168400920611     C     PULSF2        BEGSR
168500920611     C                   SETON                                        37
168600941111     C                   WRITE     TA67C07
168700920611     C                   SETOFF                                       37
168800920611     C                   ENDSR
168900920611     C*
169000920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
169100921214     C     CARSFL        BEGSR
169200920609     C                   SETOFF                                       323399
169300050704     c                   clear                   savcts
169400920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
169500920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
169600920619     C                   SETON                                        050436
169700920616     C*
169800920611     C                   MOVEL     'CT'          COD
169900920611     C                   MOVEL     *BLANKS       KEY
170000920529     C* RICERCA
170100061011     C  n12KTPT          SETLL     TITPD000
170200061011     c   12ktpt          setll     tiopd000
170300920608     C                   Z-ADD     0             NR2               4 0
170400061011     C  n12KTPT          READE     TITPD000                               32
170500061011     c   12ktpt          reade     tiopd000                               32
170600920529     C* 32 ON - FINE LETTURA
170700920611     C     *IN32         DOWEQ     '0'
170800020207      *
170900050704     C                   SETOFF                                       767769
171000020207      *
171100920611     C* CONTROLLO VALIDITA' TARIFFA
171200920611     C     TPDATB        IFEQ      ' '
171300920529     C* CODICE TASSAZIONE
171400920529     C                   MOVEL     TPDCTS        VI2CTS
171500920608     C* DECODIFICA CODICE TASSAZIONE
171600920609     C                   MOVEL     VI2CTS        KEY
171700920608     C     KTAB          CHAIN     TABEL                              33
171800920612     C  N33              DO
171900920612     C                   MOVEL     TBLUNI        DSCT
172000920612     C                   MOVEL     §CTDES        VI2DCT
172100920612     C                   END
172200920608     C   33              MOVEL     *BLANKS       VI2DCT
172300020207      *
172400020207      * Se è tariffa FedEx devo copiare solo i codici tassazioni
172500020207      * previsti per FedEx
172600020207     C     FLGNET        IFEQ      'F'
172700020207     C     §CTUTF        ANDNE     'S'
172800020207     C                   SETON                                        76
172900020207     C     VI2MSG        IFEQ      *BLANKS
173000020207     C                   SETON                                        13
173100020207     C                   MOVEA     MSG(2)        VI2MSG
173200020207     C                   ENDIF
173300020207     C                   ENDIF
173400020207      * Se NON è tariffa FedEx devo copiare solo i codici tassazioni
173500020207      * NON previsti per FedEx
173600020207     C     FLGNET        IFNE      'F'
173700020207     C     §CTUTF        ANDEQ     'S'
173800020207     C                   SETON                                        77
173900020207     C     VI2MSG        IFEQ      *BLANKS
174000020207     C                   SETON                                        13
174100020207     C                   MOVEA     MSG(3)        VI2MSG
174200020207     C                   ENDIF
174300020207     C                   ENDIF
174400050704      *
174500050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
174600050704      * per codice di tassazione
174700050704     c                   If        vi2tpg = '2'
174800050704     c                   If        tpdcts = savcts
174900050704     c                   Eval      *In69 = *On
175000050704     c                   If        vi2msg = *Blanks
175100050704     c                   Eval      *In13 = *On
175200050704     c                   Movea     msg(6)        vi2msg
175300050704     c                   EndIf
175400050704     c                   Else
175500050704     c                   Eval      savcts = tpdcts
175600050704     c                   EndIf
175700050704     c                   EndIf
175800020207      *
175900970318     C                   MOVEL     TPDORP        VI2ORP
176000920615     C* SCAGLIONE
176100940712     C                   MOVEL     TPDSGL        VI2SGL
176200920608     C* IMPORTO TARIFFA
176300941110     C                   Z-ADD     TPDITR        VI2ITR
176400941110     C                   Z-ADD     TPDITR        SAVITR
176500960520     C                   Z-ADD     TPDITR        SA2ITR
176600920608     C* MINIMO TARIFFA
176700990614     C                   Z-ADD     TPDMIN        VI2MIN
176800990614     C                   Z-ADD     TPDMIN        SAVMIN
176900940713     C* MASSIMO TARIFFA
177000990614     C                   Z-ADD     TPDMAX        VI2MAX
177100990614     C                   Z-ADD     TPDMAX        SAVMAX
177200920611     C* 06 ON - TIPO CONSEGNA = I  CARICO FLAG TARIFFA PROVINCIA
177300920611     C   06              MOVEL     TPDAIN        VI2AIN
177400930219     C   06              MOVEL     TPDAIN        SAVAIN
177500930219     C  N06              MOVEL     ' '           SAVAIN
177600920610     C*
177700061011     C  n12              Z-ADD     TPDNRR        VI2NRR
177800061011     c   12              z-add     tponrr        vi2nrr
177900920608     C* FLAG ANNULLAMENTO
178000920610     C                   MOVEL     ' '           VI2ATB
178100920610     C*
178200920608     C                   ADD       1             NR2
178300920608     C*
178400941111     C                   WRITE     TA67S07
178500920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
178600920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
178700920709     C                   SETOFF                                       0436
178800920611     C                   END
178900920608     C*
179000061011     C  n12KTPT          READE     TITPD000                               32
179100061011     c   12ktpt          reade     tiopd000                               32
179200920611     C                   END
179300920608     C*
179400920609     C                   Z-ADD     NR2           SAVNR2            4 0
179500920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
179600920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
179700920619     C                   SETOFF                                       0436
179800920608     C*
179900920616     C     NR2           IFGT      0
180000920608     C     NR2           DIV       9             DIVIS             3 0
180100920608     C                   MVR                     RESTO             3 0
180200920611     C     RESTO         IFGT      0
180300920611     C                   EXSR      SCRIVI
180400920611     C                   END
180500921126     C*
180600920616     C                   ELSE
180700920618     C                   SETON                                        03
180800920616     C                   EXSR      SCRIVI
180900920616     C                   END
181000920611     C*
181100920608     C                   ENDSR
181200920608     C*
181300920609     C*--- CARICO DATI SUBFILE CONTROL -------------------------------*
181400920608     C     CARCTL        BEGSR
181500061011     C  n12KTPT          CHAIN     TITPT000                           33
181600061011     c   12ktpt          chain     tiopt000                           33
181700941111    1C     *IN33         IFEQ      '0'
181800140115     c                   eval      dTPT01 = TPTflo
181900920925     C*
182000941111     C* 26 ON  - VISUALIZZO ARROTONDAMENTI
182100941111    2C     *IN26         IFEQ      *ON
182200990614     C                   Z-ADD     TPTARL        VI2ARL
182300990614     C                   Z-ADD     TPTARF        VI2ARF
182400990614     C                   Z-ADD     TPTARO        VI2ARO
182500941111    2C                   END
182600941111     C* 27 ON  - VISUALIZZO RAPPORTO PESO/VOLUME
182700920925     C   27              Z-ADD     TPTRPV        VI2RPV
182800920924     C*
182900941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
183000941111    2C     *IN11         IFEQ      *ON
183100920924     C* VALORE MERCE/LIMITE RISARCIBILE
183200920924     C                   Z-ADD     TPTVLM        VI2VLM
183300921009     C                   Z-ADD     TPTVLM        SAVVLM
183400941111     C* TIPO VALORE MERCE
183500940712     C                   MOVEL     TPTFVM        VI2FVM
183600941111     C* DECODIFICA TIPO VALORE MERCE
183700921007     C                   MOVEL     *BLANKS       DESFVM
183800980316     C                   MOVEL     'TR'          COD
183900921007     C                   MOVEL     *BLANKS       KEY
184000921007     C                   MOVEL     VI2FVM        KEY
184100921007     C     KTAB          CHAIN     TABEL                              34
184200941111    3C     *IN34         IFEQ      '0'
184300980316     C                   MOVEL     TBLUNI        DSTR
184400980316     C                   MOVEL     §TRDES        DESFVM
184500941111    3C                   END
184600941111    2C                   END
184700941111     C*
184800941111     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
184900941111     C   28              MOVEL     TPTAPL        VI2APL
185000990217     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO
185100990903     C   43              MOVEL     TPTTMA        VI2MAS
185200140219      * 82 ON  - Visualizzo il prezzo base di riferimento del carburante
185300140219      *          E IL RELATIVO scaglione e la data non visualizzata
185400060802     c                   If        *in82
185500150217     c                   z-add     tptdpb        vo2dpb
185600140115     c                   IF        %check(c_Digits:%subst(TPTflo:1:
185700140115     c                             %len(§TPTpma))) = *zeros
185800140115     c                   eval      VI2pma = §TPTpma
185900140115     c                   ENDIF
186000060802     c                   endif
186100150217      *
186200150219      * -?Visualizzazione "Applicazione VMA" (_/N)?
186300150219     c                   eval      VO2vma = §TPTvma
186400150219      *
186500150219     c                   clear                   dVMA
186600150219     c                   If        VI2ftc = 'f '
186700150219     c                   clear                   dsBS02
186800150219     c                   eval      T02mod = 'C'
186900150219     c                   eval      T02sif = knsif
187000150219     c                   eval      T02cod = 'VMA'
187100150219     c  n12              movel     VIDcli        T02ke1
187200150219     c   12              movel     VISksc        T02ke1
187300150219     c                   call      'TIBS02R'
187400150219     c                   parm                    kpjba
187500150219     c                   parm                    dsBS02
187600150219     c                   if        T02err = *blanks
187700150219     c                   eval      dVMA   = T02uni
187800150219     c                   endif
187900150219     c                   EndIf
188000150219     c                   eval      *in90 = ( VI2ftc =  'f '     and
188100150219     c                                       VO2vma =  'N'      and
188200150219     c                                       PARddt >= §VMAddt  and
188300150219     c                                       PARddt <= §VMAdst )
188400150217      *
188500131216      * recupero il prezzo del gasolio corrispondente
188600131218     c                   eval      Kpmg = TPTdpb
188700131218     c                   exsr      Rec_pmg
188800131218
188900941111    1C                   END
189000060802     C
189100920924     C*
189200920608     C                   ENDSR
189300920609     C*
189400920609     C*--- SR AGGIUNTA RIGHE -----------------------------------------*
189500920609     C     SCRIVI        BEGSR
189600920611     C                   SETOFF                                       05
189700020207     C                   SETOFF                                       7677
189800920611     C                   Z-ADD     0             B                 3 0
189900920611     C     RESTO         IFNE      0
190000920611     C                   Z-ADD     RESTO         B
190100920611     C                   Z-ADD     0             RESTO
190200920611     C                   END
190300920609     C* CAMPI DEL SUBFILE
190400920609     C                   Z-ADD     SAVNR2        NR2
190500060306
190600060306      * se tariffa particolare "d" imposto i dati di dft
190700140128      * + il prezzo recuperato dal file TIACB
190800060306      * e scrivo una riga
190900060306     c                   if        vidftc = 'd ' and *in01
191000060307     c                   eval      *in79 = *on
191100060306     c                   eval      vi2cts = 'IT'
191200060306     c                   eval      vi2sgl = 9999999
191300140128     c******             select
191400060306      * clienti Top o Direzionali
191500140128     c******             when      clpclv = 'T' or clpclv = 'D'
191600111130     c****               eval      vi2itr = 0,002
191700140128     c******             eval      vi2itr = 0,003
191800060306      * clienti A
191900140128     c******             when      clpclv = 'A'
192000111130     c****               eval      vi2itr = 0,003
192100140128     c******             eval      vi2itr = 0,004
192200060306      * clienti C o B
192300140128     c******             when      clpclv = 'C' or clpclv = 'B'
192400111130     c****               eval      vi2itr = 0,005
192500140128     c******             eval      vi2itr = 0,007
192600060314      * non trovo la categoria cliente imposto il massimo
192700140128     c******             other
192800111130     c****               eval      vi2itr = 0,005
192900140128     c******             eval      vi2itr = 0,007
193000140128     c******             endsl
193100140128
193200140128       //?Recupero la tariffa AC BASE dal file TIACB
193300140128     c     kACB          setll     TIACB01L
193400140128     c     CLPclv        reade     TIACB01L
193500140128     c                   if        %found(TIACB01L)
193600140128     c                   eval      VI2itr = ACBitr
193700140128     c                   ENDIF
193800140128
193900060705      * controllo se il cliente è presente sulla tabella della forzatura
194000060705      * AC BASE e recupero la tariffa dalla tabella
194100060705     c                   clear                   dsbs02
194200060705     c                   clear                   dfab
194300060705     c                   eval      t02mod = 'C'
194400060705     c                   eval      t02sif = knsif
194500060705     c                   eval      t02cod = 'FAB'
194600060705     c  n12              movel     vidcli        t02ke1
194700060705     c   12              movel     visksc        t02ke1
194800060705     c                   call      'TIBS02R'
194900060705     c                   parm                    kpjba
195000060705     c                   parm                    dsbs02
195100060705     c                   if        t02err = *blanks
195200060705     c                   eval      dfab = t02uni
195300060705     c                   endif
195400060705     c                   if        d§fabitr > 0
195500060705     c                   eval      vi2itr = d§fabitr
195600060705     c                   endif
195700060705
195800060306     c                   z-add     0,001         vi2min
195900060306     c                   z-add     0             vi2max
196000060306     c                   z-add     vi2itr        savitr
196100060306     c                   z-add     vi2itr        sa2itr
196200060306     c                   z-add     vi2min        savmin
196300060306     c                   z-add     0             savmax
196400060306     c                   z-add     0             vi2nrr
196500060306     c                   add       1             nr2
196600060306     c                   add       1             b
196700060306     c                   write     ta67s07
196800060306     c                   setoff                                       0336
196900060306     c                   endif
197000920609     C*
197100920625     C     B             DOWLT     9
197200920609     C                   MOVEL     *BLANKS       VI2CTS
197300920609     C                   MOVEL     *BLANKS       VI2DCT
197400940712     C                   Z-ADD     0             VI2SGL
197500940712     C                   Z-ADD     0             VI2ITR
197600940712     C                   Z-ADD     0             VI2MIN
197700940713     C                   Z-ADD     0             VI2MAX
197800920611     C                   MOVEL     ' '           VI2ATB
197900920609     C* 06 ON - TIPO CONSEGNA = I
198000920610     C   06              MOVEL     'S'           VI2AIN
198100920612     C                   MOVEL     *BLANKS       VI2ORP
198200940712     C                   Z-ADD     0             SAVITR
198300960520     C                   Z-ADD     0             SA2ITR
198400940712     C                   Z-ADD     0             SAVMIN
198500940713     C                   Z-ADD     0             SAVMAX
198600930219     C   06              MOVEL     'S'           SAVAIN
198700930219     C  N06              MOVEL     ' '           SAVAIN
198800920609     C                   Z-ADD     0             VI2NRR
198900941116     C*
199000920609     C                   ADD       1             NR2
199100920609     C                   ADD       1             B
199200941111     C                   WRITE     TA67S07
199300920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
199400920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
199500920619     C                   SETOFF                                       0336
199600920609     C                   END
199700920609     C*
199800920609     C                   Z-ADD     NR2           SAVNR2
199900920609     C                   Z-ADD     NR2           REC2
200000920609     C                   ENDSR
200100920611     C*
200200920611     C*--- ANNULLAMENTO TOTALE TARIFFA -------------------------------*
200300920611     C     ANNTAR        BEGSR
200400920709     C                   SETOFF                                       32
200500920611     C* ANNULLO DETTAGLIO
200600061011     C  n12KTPT          SETLL     TITPD000
200700061011     c   12ktpt          setll     tiopd000
200800920611     C     *IN32         DOWEQ     '0'
200900061011     C  n12KTPT          READE     TITPD000                               32
201000061011     c   12ktpt          reade     tiopd000                               32
201100940713     C  N32TPDATB        IFEQ      ' '
201200941109     C     *IN12         IFEQ      *ON
201300941109     C* OFFERTA
201400061011     C                   DELETE    tiopd000
201500941109     C                   ELSE
201600941109     C* TARIFFA
201700941109     C                   MOVEL     'A'           TPDATB
201800941109     C                   MOVEL     ' '           TPDFTR
201900040915     C                   Z-ADD     dateu8        TPDDTR
202000990614     C                   UPDATE    TITPD000
202100941109     C                   END
202200921217     C                   END
202300920611     C                   END
202400921130     C*
202500940713     C* ANNULLO TESTATA
202600061011     C  n12KTPT          CHAIN     TITPT000                           31
202700061011     c   12ktpt          chain     tiopt000                           31
202800940713     C     *IN31         IFEQ      *OFF
202900941109     C     *IN12         IFEQ      *ON
203000941109     C* OFFERTA
203100061011     C                   DELETE    tiopt000
203200941109     C                   ELSE
203300941109     C* TARIFFA
203400990615     C* DATA ULTIMA VARIAZIONE
203500990615     C                   Z-ADD     DATEU8        TPTDUV
203600941109     C                   MOVEL     'A'           TPTATB
203700941109     C                   MOVEL     ' '           TPTFTR
203800040915     C                   Z-ADD     dateu8        TPTDTR
203900990614     C                   UPDATE    TITPT000
204000030715      * mi salvo la tariffa particolare annullata
204100030715     c                   movel(p)  TPTftc        w_manftc
204200030715     c                   move      'A'           W_manftc
204300030715     c                   z-add     1             WW
204400030715     c     w_manftc      lookup    manftc                                 87
204500030715     c                   if        not %found
204600030715     c     '   '         lookup    manftc(ww)                             87
204700030715     c                   if        %found
204800030715     c                   movea     w_manftc      manftc(ww)
204900030715     c                   endif
205000030715     c                   endif
205100941109     C                   END
205200940713     C                   END
205300921130     C*
205400921217     C*
205500920618     C                   SETON                                        09
205600920611     C                   ENDSR
205700920609     C*
205800920609     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
205900920609     C     CONSFL        BEGSR
206000920619     C                   SETOFF                                       993304
206100140131     c                   setoff                                       7976
206200920609     C                   Z-ADD     1             NR2
206300031003     C* INDICATORE 62 -> RIMANE ACCESO SE NON CI SONO RECORD VALORIZZATI
206400031003     C*                  NEL SUBFILE
206500031003     C                   SETON                                        62
206600140131
206700140131     c                   If        vidftc = 'f '
206800140131     C* INDICATORE 88 -> RIMANE ACCESO SE NON CI SONO scaglioni > = 9999
206900140131     C*                  NEL SUBFILE  solo per il fuel
207000140131     C                   SETON                                        88
207100140131
207200140131     c                   else
207300140219     c                   setoff                                       88
207400140131     c                   Endif
207500920609     C*
207600941111     C     NR2           CHAIN     TA67S07                            33
207700941111     C*
207800941111    1C     *IN33         DOWEQ     '0'
207900920624     C   15              MOVEL     DUPCTS        VI2CTS
208000940712     C   16              Z-ADD     DUPSGL        VI2SGL
208100940712     C   17              Z-ADD     DUPITR        VI2ITR
208200940712     C   18              Z-ADD     DUPMIN        VI2MIN
208300940713     C   23              Z-ADD     DUPMAX        VI2MAX
208400920624     C   19              MOVEL     DUPAIN        VI2AIN
208500920616     C*
208600941111    2C     VI2CTS        IFNE      *BLANKS
208700940713     C     RECORD        ORNE      *BLANKS
208800940713     C     RECORD        ANDNE     *ZEROS
208900031003      *
209000031003     C                   SETOFF                                       62
209100941111     C*
209200920609     C* CONTROLLO SE RECORD DI NUOVA IMMISSIONE
209300941111    3C     VI2NRR        IFEQ      0
209400920609     C                   SETOFF                                       05
209500920827     C*
209600920827     C*** CODICE TASSAZIONE
209700920609     C* CODICE TASSAZIONE OBBLIGATORIO
209800941111    4C     VI2CTS        IFEQ      *BLANKS
209900920611     C                   SETON                                        4599
210000920611     C                   GOTO      ENDSFL
210100941111    4C                   END
210200010920     C* CODICE TASSAZIONE "EE" ERRATO
210300020328      * se tariffa DPD
210400010920    4C     VI2CTS        IFEQ      'EE'
210500020325     C     FLGNET        ANDEQ     'D'
210600011022     C                   SETON                                        741399
210700011022     C                   MOVEA     MSG(1)        VI2MSG
210800011022     C                   GOTO      ENDSFL
210900010920    4C                   ENDIF
211000020328
211100020328      * se tariffa italia non accetto codice tassazione 'EE'
211200020328     c                   if        parfie = 'I' and vi2cts = 'EE'
211300020328     c                   eval      *in74 = *on
211400020328     c                   eval      *in13 = *on
211500020328     c                   eval      *in99 = *on
211600020328     c                   eval      vi2msg = msg(1)
211700020328     c                   goto      endsfl
211800020328     c                   endif
211900020328      * se tariffa estera non accetto codice tassazione 'IT'
212000020328     c                   if        parfie = 'E' and vi2cts = 'IT'
212100020328     c                   eval      *in74 = *on
212200020328     c                   eval      *in13 = *on
212300020328     c                   eval      *in99 = *on
212400020328     c                   eval      vi2msg = msg(4)
212500020328     c                   goto      endsfl
212600020328     c                   endif
212700020328
212800941020     C* SE LA CONSEGNA PARTICOLARE IN GESTIONE RICHIEDE UN PARTICOLARE
212900941111     C*   CODICE DI TASSAZIONE E' NECESSARIO CHE SIA PRESENTE SOLO
213000941020     C*   QUELLO NELLE RIGHE DI DETTAGLIO
213100950119    4C     WCTS          IFNE      *BLANKS
213200950119     C     WCTE          ANDNE     *BLANKS
213300941123     C*
213400950119    5C     WCTS          IFNE      VI2CTS
213500950119     C     WCTE          ANDNE     VI2CTS
213600950119     C                   MOVEA     WCTS          MS2(49)
213700950119     C                   MOVEA     WCTE          MS2(56)
213800950119     C                   MOVEA     MS2           VI2MSG
213900950119     C                   SETON                                        741399
214000941020     C                   GOTO      ENDSFL
214100941123    5C                   ENDIF
214200941123     C*
214300941123   X4C                   ELSE
214400941123     C*
214500941123    5C     WCTS          IFNE      *BLANKS
214600941123     C     WCTS          ANDNE     VI2CTS
214700950119     C                   MOVEA     WCTS          MS1(50)
214800950119     C                   MOVEA     MS1           VI2MSG
214900950119     C                   SETON                                        741399
215000941123     C                   GOTO      ENDSFL
215100941123    5C                   ENDIF
215200950119     C*
215300950119    5C     WCTE          IFNE      *BLANKS
215400950119     C     WCTE          ANDNE     VI2CTS
215500950119     C                   MOVEA     WCTE          MS1(50)
215600950119     C                   MOVEA     MS1           VI2MSG
215700950119     C                   SETON                                        741399
215800950119     C                   GOTO      ENDSFL
215900950119    5C                   ENDIF
216000941123    4C                   ENDIF
216100120713
216200120713      * ricerca codice tassazione
216300120713     c                   IF        %scan('?':VI2cts) > 0
216400120713     c                   clear                   TRTB09DS
216500120713     c                   eval      TB09fun = '1'
216600120713     c                   eval      TB09ord = '1'
216700120713     c                   IF        flgnet = 'D'
216800120713     c                   eval      TB09est = 'E'
216900120713     c                   ELSE
217000120713     c                   eval      TB09est = parfie
217100120713     c                   ENDIF
217200120713     c                   eval      TB09uta = 'S'
217300120713     c                   IF        flgnet = 'F'
217400120713     c                   eval      TB09utf = 'S'
217500120713     c                   ELSE
217600120713     c                   eval      TB09utf = 'N'
217700120713     c                   ENDIF
217800120713     c                   eval      kpjbu = TRTB09DS
217900120713     c                   call      'TRTB09R'
218000120713     c                   parm                    kpjba
218100120713     c                   eval      TRTB09DS = kpjbu
218200120713     c                   IF        TB09cod <> *blanks
218300120713     c                   eval      VI2cts = TB09cod
218400120713     c                   ELSE
218500120723     c                   clear                   VI2cts
218600120713     c                   ENDIF
218700120713     c                   ENDIF
218800941123     C*
218900920609     C* SE IMMESSO COD.TASSAZIONE CONTROLLO SE ESISTE
219000920609     C                   MOVEL     'CT'          COD
219100920611     C                   MOVEL     *BLANKS       KEY
219200920609     C                   MOVEL     VI2CTS        KEY
219300920609     C     KTAB          CHAIN     TABEL                              34
219400920611     C* CONTROLLO VALIDITA' CODICE TASSAZIONE
219500941111    4C  N34TBLFLG        IFNE      *BLANKS
219600920611     C                   SETON                                        4699
219700920611     C                   GOTO      ENDSFL
219800941111   X4C                   ELSE
219900950112     C                   MOVEL     TBLUNI        DSCT
220000950112     C*
220100950112     C*
220200920611     C* DECODIFICO CODICE TASSAZIONE
220300920617     C                   MOVEL     §CTDES        VI2DCT
220400990614     C                   MOVE      §CTCOR        VI2ORP
220500941111    4C                   END
220600950112     C*
220700920611     C* 34 ON - CODICE TASSAZIONE INESISTENTE
220800920611     C   34              SETON                                        4799
220900920611     C   34              GOTO      ENDSFL
221000050304      * errore bloccante se codice tassazione non utilizzabile in gestione
221100050304      * tariffe clienti
221200050304     c                   If        §ctuta = 'N'
221300050304     c                   Eval      *In47 = *On
221400050304     c                   Eval      *In99 = *On
221500050304     c                   Goto      endsfl
221600050304     c                   EndIf
221700020207      *
221800020207      * Se è tariffa FedEx devo copiare solo i codici tassazioni
221900020207      * previsti per FedEx
222000020207     C     FLGNET        IFEQ      'F'
222100020207     C     §CTUTF        ANDNE     'S'
222200020207     C                   SETON                                        769913
222300020207     C                   MOVEA     MSG(2)        VI2MSG
222400020207     C                   GOTO      ENDSFL
222500020207     C                   ENDIF
222600020207      * Se NON è tariffa FedEx devo copiare solo i codici tassazioni
222700020207      * NON previsti per FedEx
222800020207     C     FLGNET        IFNE      'F'
222900020207     C     §CTUTF        ANDEQ     'S'
223000020207     C                   SETON                                        779913
223100020207     C                   MOVEA     MSG(3)        VI2MSG
223200020207     C                   GOTO      ENDSFL
223300020207     C                   ENDIF
223400070226      * Se tariffa estero accetto solo estero
223500070226     c                   if        parfie = 'E' and §ctest = *Blanks
223600070226     c                   eval      *in13 = *on
223700070226     c                   eval      *in76 = *on
223800070226     c                   eval      *in99 = *on
223900070226     c                   movea     msg(12)       vi2msg
224000070226     c                   goto      endsfl
224100070226     c                   endif
224200070226      * Se tariffa DPD accetto solo estero
224300070226      * tranne il codice generico IT xchè va bene
224400070226     c                   if        flgnet = 'D' and §ctest = *Blanks and
224500070227     c                             vi2cts <> 'IT'
224600070226     c                   eval      *in13 = *on
224700070226     c                   eval      *in76 = *on
224800070226     c                   eval      *in99 = *on
224900070226     c                   movea     msg(12)       vi2msg
225000070226     c                   goto      endsfl
225100070226     c                   endif
225200070226      * Se tariffa italia accetto solo italia
225300070226     c                   if        parfie = 'I' and flgnet <> 'D' and
225400070226     c                                              §ctest <> *Blanks
225500070226     c                   eval      *in13 = *on
225600070226     c                   eval      *in76 = *on
225700070226     c                   eval      *in99 = *on
225800070226     c                   movea     msg(12)       vi2msg
225900070226     c                   goto      endsfl
226000070226     c                   endif
226100050704
226200050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
226300050704      * per codice di tassazione
226400050704     c                   If        vi2tpg = '2' and vi2atb = *Blanks
226500061011     c  n12ktpd01        Setll     titpd01l
226600061011     c   12ktpd01        Setll     tiopd01l
226700050704     c                   Do        *Hival
226800061011     c  n12ktpd01        Reade(n)  titpd01l
226900061011     c   12ktpd01        Reade(n)  tiopd01l
227000061011     c                   If        %eof
227100050704     c                   Leave
227200050704     c                   EndIf
227300050704     c                   If        tpdatb <> *Blanks
227400050704     c                   Iter
227500050704     c                   EndIf
227600050704     c                   Eval      *in74 = *on
227700050704     c                   Eval      *in13 = *on
227800050704     c                   Eval      *in99 = *on
227900050704     c                   Eval      vi2msg = msg(6)
228000050704     c                   Goto      endsfl
228100050704     c                   EndDo
228200050704     c                   EndIf
228300020207      *
228400920827     C*
228500920827     C*** SCAGLIONE OBBLIGATORIO
228600941111    4C     VI2SGL        IFEQ      0
228700920609     C                   SETON                                        4899
228800920609     C                   GOTO      ENDSFL
228900950119   X4C                   ELSE
229000990622     C* SE TIPO TARIFFA/PAGAMENTO E' A VALORE LO SCAGLIONE PUO' ESSERE UN VALORE
229100990622     C* ANCHE CON I DECIMALI IN BASE ALLA DIVISA
229200990622    5C     *IN14         IFEQ      *ON
229300990622     C                   MOVE      VI2SGL        W0030
229400990622    6C     W0030         IFGT      0
229500990622     C     FLGDEC        ANDNE     'S'
229600990622     C                   SETON                                        7199
229700990622     C                   GOTO      ENDSFL
229800990622    6C                   ENDIF
229900990622     C**
230000990622   X5C                   ELSE
230100950119     C* WSCD = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
230200950119     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
230300990622    6C     WSCD          IFEQ      ' '
230400950119     C                   MOVE      VI2SGL        W0030
230500950119     C     W0030         IFGT      0
230600950119     C                   SETON                                        7199
230700950119     C                   GOTO      ENDSFL
230800950119     C                   ENDIF
230900990622    6C                   ENDIF
231000990622    5C                   ENDIF
231100080922    4C                   END
231200920827     C*
231300920827     C*** CONTROLLO SE RIGHE DOPPIE
231400061011     C  n12KTPD          CHAIN     TITPD000                           32
231500061011     c   12ktpd          chain     tiopd000                           32
231600061011     c  n32
231700061011    4CanN12TPDNRR        IFNE      VI2NRR
231800941111     C     TPDATB        ANDEQ     ' '
231900920709     C                   SETON                                        4999
232000920709     C                   GOTO      ENDSFL
232100941111    4C                   END
232200061011     c  n32
232300061011    4can 12tponrr        ifne      VI2NRR
232400061011     C     TPDATB        ANDEQ     ' '
232500061011     C                   SETON                                        4999
232600061011     C                   GOTO      ENDSFL
232700061011    4C                   END
232800920828     C*
232900941111   X3C                   ELSE
233000920617     C                   SETON                                        05
233100941111    3C                   END
233200060307
233300060307      * se tariffa particolare 'd ' AC BASE non posso andare al di sotto del
233400060307      * minimo indicato in DV 1463
233500060705      * o in tabella FAB
233600060313     c                   If        vidftc = 'd '
233700060705      * controllo se il cliente è presente sulla tabella della forzatura
233800060705      * AC BASE e controllo la tariffa inserita con quanto presente in tabella
233900060705     c                   clear                   dsbs02
234000060705     c                   clear                   dfab
234100060705     c                   eval      t02mod = 'C'
234200060705     c                   eval      t02sif = knsif
234300060705     c                   eval      t02cod = 'FAB'
234400060705     c  n12              movel     vidcli        t02ke1
234500060705     c   12              movel     visksc        t02ke1
234600060705     c                   call      'TIBS02R'
234700060705     c                   parm                    kpjba
234800060705     c                   parm                    dsbs02
234900060705     c                   if        t02err = *blanks
235000060705     c                   eval      dfab = t02uni
235100060705     c                   endif
235200060705     c                   if        vi2itr < d§fabitr
235300060705     c                   eval      vi2msg = msg(11)
235400060705     c                   eval      %subst(vi2msg:38:12) = %editc(d§fabitr:'4')
235500060705     c                   eval      *in13 = *on
235600060705     c                   eval      *in79 = *on
235700060705     c                   eval      *in99 = *on
235800060705     c                   goto      endsfl
235900060705     c                   endif
236000060705      * se cliente presente in tabella FAB non faccio gli altri controlli
236100060705     c                   if        d§fabitr = *zeros
236200140128
236300140128       //?Recupero la tariffa AC BASE dal file TIACB
236400140128     c     kACB          setll     TIACB01L
236500140128     c     CLPclv        reade     TIACB01L
236600140128     c                   IF        %found(TIACB01L) and
236700140128     c                             VI2itr < ACBitr
236800140128     c                   eval      vi2msg = msg(7)
236900140128     c                   eval      %subst(vi2msg:10:1) = clpclv
237000140128     c                   eval      VI2msg = %trim(VI2msg) + ' ' +
237100140128     c                             %trim(%editc(ACBitr:'4'))
237200140128     c                   eval      *in13 = *on
237300140128     c                   eval      *in79 = *on
237400140128     c                   eval      *in99 = *on
237500140128     c                   goto      endsfl
237600140128     c                   ENDIF
237700140128
237800140128     c******             Select
237900060307      * clienti Top o Direzionali
238000140128     c******             when      (clpclv = 'T' or clpclv = 'D') and
238100140128     c******                       vi2itr < 0,003
238200111130     c****                         vi2itr < 0,002
238300140128     c******             eval      vi2msg = msg(7)
238400111130     c****               eval      %subst(vi2msg:50:5) = '0,002'
238500140128     c******             eval      %subst(vi2msg:50:5) = '0,003'
238600140128     c******             eval      %subst(vi2msg:10:1) = clpclv
238700140128     c******             eval      *in13 = *on
238800140128     c******             eval      *in79 = *on
238900140128     c******             eval      *in99 = *on
239000140128     c******             goto      endsfl
239100060307      * clienti A
239200140128     c******             when      clpclv = 'A' and
239300140128     c******                       vi2itr < 0,004
239400111130     c****                         vi2itr < 0,003
239500140128     c******             eval      vi2msg = msg(7)
239600111130     c****               eval      %subst(vi2msg:50:5) = '0,003'
239700140128     c******             eval      %subst(vi2msg:50:5) = '0,004'
239800140128     c******             eval      %subst(vi2msg:10:1) = clpclv
239900140128     c******             eval      *in13 = *on
240000140128     c******             eval      *in79 = *on
240100140128     c******             eval      *in99 = *on
240200140128     c******             goto      endsfl
240300060307      * clienti C o B
240400140128     c******             when      (clpclv = 'C' or clpclv = 'B') and
240500140128     c******                       vi2itr < 0,007
240600111130     c****                         vi2itr < 0,005
240700140128     c******             eval      vi2msg = msg(7)
240800111130     c****               eval      %subst(vi2msg:50:5) = '0,005'
240900140128     c******             eval      %subst(vi2msg:50:5) = '0,007'
241000140128     c******             eval      %subst(vi2msg:10:1) = clpclv
241100140128     c******             eval      *in13 = *on
241200140128     c******             eval      *in79 = *on
241300140128     c******             eval      *in99 = *on
241400140128     c******             goto      endsfl
241500140128     c******             endsl
241600060705     c                   endif
241700060313     c                   endif
241800140115
241900140131      //?Se tariffa particolare 'f ' FUEL se importo > 0 o scaglione >=9999
242000140131      //?spendo INDICATORI DI ERRORE
242100140131     c                   IF        VIDftc = 'f ' and
242200140131     c                             VI2sgl >= 9999
242300140220     c                             and  VI2atb = ' '
242400140131     c                   setoff                                       88
242500140131     c                   ENDIF
242600140115
242700920827     C*
242800930219     C*** PER TUTTE CONSEGNE PARTICOLARI SI PUO' ACCETTARE
242900921202     C*     L'IMPORTO TARIFFA = 0  CON IL CMD14 DI FORZATURA
243000941111    3C     VI2ITR        IFEQ      0
243100930219     C  NKN              SETON                                        6199
243200921202     C   99              GOTO      ENDSFL
243300960520     C*
243400941111   X3C                   ELSE
243500960520     C* CONTROLLO CHE NON SUPERI LA MASSIMA % PER LA TARIFFA A VALORE:
243600960520     C*   SE LA SUPERA EMETTO MESSAGGIO FORZABILE
243700941111    4C     *IN14         IFEQ      *ON
243800960520     C*
243900960704    5C     VI2ITR        IFNE      SA2ITR
244000960520     C     VI2ITR        ANDGT     WPTV
244100941110     C                   SETON                                        6699
244200960704     C                   MOVEL     VI2ITR        SA2ITR
244300941110     C                   GOTO      ENDSFL
244400941111    5C                   END
244500941111     C*
244600941111   X4C                   ELSE
244700941110     C* SE SI TRATTA DI TARIFFA IN £ NON SI POSSONO INSERIRE I DECIMALI
244800941111     C                   MOVE      VI2ITR        W0030             3 0
244900941111    5C     W0030         IFGT      0
245000990622     C     FLGDEC        ANDNE     'S'
245100941110     C                   SETON                                        6899
245200941110     C                   GOTO      ENDSFL
245300941111    5C                   END
245400941111    4C                   END
245500941111    3C                   END
245600990622     C*
245700990622     C*** IMPORTO MINIMO  TARIFFA
245800990622     C* CONTROLLO I DECIMALI
245900990622    3C     VI2MIN        IFGT      0
246000990622     C                   MOVE      VI2MIN        W0030             3 0
246100990622    4C     W0030         IFGT      0
246200990622     C     FLGDEC        ANDNE     'S'
246300990622     C                   SETON                                        6499
246400990622     C                   GOTO      ENDSFL
246500990622    4C                   END
246600990622    3C                   END
246700990622     C*
246800990622     C*** IMPORTO MASSIMO TARIFFA
246900990622     C* CONTROLLO I DECIMALI
247000990622    3C     VI2MAX        IFGT      0
247100990622     C                   MOVE      VI2MAX        W0030             3 0
247200990622    4C     W0030         IFGT      0
247300990622     C     FLGDEC        ANDNE     'S'
247400990622     C                   SETON                                        6599
247500990622     C                   GOTO      ENDSFL
247600990622    4C                   END
247700990622    3C                   END
247800920827     C*
247900940713     C*** IMPORTO MASSIMO TARIFFA
248000940713     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
248100941111    3C     VI2MAX        IFGT      0
248200940713     C     VI2MAX        ANDLT     VI2MIN
248300940713     C                   SETON                                        6399
248400940713     C                   GOTO      ENDSFL
248500941111    3C                   END
248600940713     C*
248700920616     C* SE NON CI SONO ERRORI AGGIORNO E VADO AVANTI A LEGGERE
248800920616     C                   MOVEL     VI2CTS        DUPCTS
248900940712     C                   Z-ADD     VI2SGL        DUPSGL
249000941110     C                   Z-ADD     VI2ITR        DUPITR
249100940712     C                   Z-ADD     VI2MIN        DUPMIN
249200940713     C                   Z-ADD     VI2MAX        DUPMAX
249300920616     C   06              MOVEL     VI2AIN        DUPAIN
249400941111     C*
249500941111   X2C                   ELSE
249600930225     C                   SETOFF                                       05
249700941111    2C                   END
249800920616     C*
249900920611     C     ENDSFL        TAG
250000920610     C   99              Z-ADD     NR2           REC2
250100920619     C*
250200941111     C                   UPDATE    TA67S07
250300920619     C   99              SETON                                        33
250400920619     C*
250500921126     C                   ADD       1             NR2
250600941111     C  N99NR2           CHAIN     TA67S07                            33
250700941111    1C                   END
250800031003      * SE E' ACCESO L'INDICATORE 62 SEGNALO ERRORE CHE E' OBBLIGATORIO IL
250900031003      * DETTAGLIO TARIFFA
251000140219     C                   IF        *IN62 or *in88
251100031003     C                   SETON                                        99
251200031003     C                   ENDIF
251300920611     C                   ENDSR
251400920610     C*
251500920611     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
251600920611     C     CONCTL        BEGSR
251700060802     C                   SETOFF                                       999886
251800921126     C* AZZERO CAMPI DI DUP
251900940712     C                   MOVEL     *BLANKS       DUPCTS
252000940712     C                   Z-ADD     0             DUPSGL
252100941110     C                   Z-ADD     0             DUPITR
252200940712     C                   Z-ADD     0             DUPMIN
252300940713     C                   Z-ADD     0             DUPMAX
252400940712     C   06              MOVEL     *BLANKS       DUPAIN
252500060307
252600060307      * faccio subito i controlli per AC BASE e AC PLUS
252700060307
252800060307      * se tariffa particolare "d " AC BASE la tariffa non deve
252900060307      * avere il flag di rinuncia AC BASE
253000060307     c                   if        vidftc = 'd ' and parfmp = 'S'
253100060307     c                   eval      vi2msg = msg(8)
253200060307     c                   eval      *in13 = *on
253300060307     c                   eval      *in99 = *on
253400060307     c                   leavesr
253500060307     c                   endif
253600060307
253700060307      * se tariffa particolare "d " AC BASE la tariffa non deve
253800060307      * avere una tariffa particolare AC PLUS reale con applicazione completa
253900060307     c                   if        vidftc = 'd '
254000060307     c                   movel     'C '          v09ftc
254100061011     c  n12ktpt9         chain(n)  titpt01l
254200061011     c   12ktpt9         chain(n)  tiopt01l
254300061011     c                   if        %found and tptatb = *blanks and
254400060307     c                             tpttma = *blanks and tptapl = *blanks
254500060307     c                   eval      vi2msg = msg(9)
254600060307     c                   eval      *in13 = *on
254700060307     c                   eval      *in99 = *on
254800060307     c                   leavesr
254900060307     c                   endif
255000060307     c                   endif
255100060307
255200060307      * se tariffa particolare "C " AC PLUS reale completa la
255300060307      * tariffa non deve avere una tariffa particolare AC BASE
255400060307     c                   if        vidftc = 'C ' and vi2mas = *blanks and
255500060307     c                             vi2apl = *blanks
255600060307     c                   movel     'd '          v09ftc
255700061011     c  n12ktpt9         chain(n)  titpt01l
255800061011     c   12ktpt9         chain(n)  tiopt01l
255900061011     c                   if        %found and tptatb = *blanks
256000060307     c                   eval      vi2msg = msg(10)
256100060307     c                   eval      *in13 = *on
256200060307     c                   eval      *in99 = *on
256300060307     c                   leavesr
256400060307     c                   endif
256500060307     c                   endif
256600920827     C*
256700920924     C*** ARROTONDAMENTI : TUTTI <> 0  OPPURE  TUTTI = 0
256800941111    1C     VI2ARL        IFEQ      0
256900920611     C*
257000941111    2C     VI2ARF        IFNE      0
257100940712     C     VI2ARO        ORNE      0
257200920611     C                   SETON                                        4299
257300920611     C                   GOTO      ENDCON
257400941111    2C                   END
257500920611     C*
257600941111   X1C                   ELSE
257700940712     C*
257800941111    2C     VI2ARF        IFEQ      0
257900940712     C     VI2ARO        OREQ      0
258000920611     C                   SETON                                        4299
258100920611     C                   GOTO      ENDCON
258200941111    2C                   END
258300920611     C*
258400941111    1C                   END
258500920624     C*
258600950119     C* SE SONO STATI IMMESSI GLI ARROTONDAMENTI CONTROLLO SE SI TRATTA
258700950119     C*   DI SCAGLIONE SENZA DECIMALI PER CUI NON SI POSSONO INSERIRE
258800950119     C*   VALORI DOPO LA VIRGOLA
258900950119    1C     VI2ARL        IFGT      0
259000950119     C     WSCD          ANDEQ     ' '
259100950119     C                   MOVE      VI2ARL        W0030
259200950119    2C     W0030         IFGT      0
259300950119     C                   SETON                                        7299
259400950119     C                   GOTO      ENDCON
259500950119    2C                   ENDIF
259600950119     C*
259700950119     C                   MOVE      VI2ARF        W0030
259800950119    2C     W0030         IFGT      0
259900950119     C                   SETON                                        7299
260000950119     C                   GOTO      ENDCON
260100950119    2C                   ENDIF
260200950119     C*
260300950119     C                   MOVE      VI2ARO        W0030
260400950119    2C     W0030         IFGT      0
260500950119     C                   SETON                                        7299
260600950119     C                   GOTO      ENDCON
260700950119    2C                   ENDIF
260800950119    1C                   ENDIF
260900950119     C*
261000941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
261100941111    1C     *IN11         IFEQ      '1'
261200930223     C*
261300930223     C*** VALORE MERCE FORZABILE A ZERO CON ENTER
261400980309     C* SOLO SE NON SI TRATTA DI UNA TARIFFA IMPORT
261500990217    2C     *IN43         IFEQ      *ON
261600980429     C** SE SI TRATTA DI UNA MANDATO ASSICURATIVO FITTIZIO VI2MAS = N
261700980429     C** IL VALORE MERCE ASSICURABILE DEVE ESSERE A ZERO
261800981126    3C     VI2MAS        IFEQ      'F'
261900980429     C     VI2VLM        ANDGT     0
262000980429     C                   SETON                                        5299
262100980429     C                   GOTO      ENDCON
262200981126    3C                   END
262300981126    2C                   END
262400980309     C*
262500981126    2C     IMPORT        IFNE      'Y'
262600980416     C* CONTROLLO SE TARIFFA ESTERA
262700981126     C   24
262800981126    3COR 25PARFIE        IFEQ      'E'
262900980416     C* SE IL VALORE MERCE E' A ZERO IL TIPO APPLICAZIONE DEVE ESSERE SOLO
263000980416     C* IN PARTENZA
263100980416     C     VI2VLM        ANDEQ     0
263200980416     C     VI2APL        ANDNE     'P'
263300980416     C                   SETON                                        5399
263400980416     C                   GOTO      ENDCON
263500981126    3C                   END
263600980416     C*
263700981126    3C   24UNO           IFEQ      ' '
263800930223     C     VI2VLM        ANDEQ     0
263900980505     C     VI2MAS        ANDEQ     ' '
264000930223     C                   SETON                                        5099
264100930223     C                   MOVEL     '1'           UNO
264200930223     C                   GOTO      ENDCON
264300981126    3C                   END
264400981126   X2C                   ELSE
264500980309     C* VALORE MERCE
264600981126    3C   24VI2VLM        IFEQ      *ZEROS
264700980309     C                   SETON                                        5599
264800981126    3C                   END
264900980309     C* APPLICAZIONE TARIFFA SOLO IN ARRIVO
265000981126     C   24
265100981126    3COR 25VI2APL        IFNE      'A'
265200980309     C                   SETON                                        5499
265300980416     C                   GOTO      ENDCON
265400981126    3C                   END
265500980309     C*
265600981126    2C                   END
265700990817     C* VALORE MERCE VERIFICO SE HA IMPORTI CON DECIMALI
265800990817     C*
265900990817    2C     VI2VLM        IFGT      0
266000990817     C                   MOVE      VI2VLM        W0030             3 0
266100990817    3C     W0030         IFGT      0
266200990817     C     FLGDEC        ANDNE     'S'
266300990817     C                   SETON                                        6799
266400990817     C                   GOTO      ENDCON
266500990817    3C                   END
266600990817    2C                   END
266700921009     C*
266800930219     C*** LIMITE RISARCIBILE NON PUO' ESSERE MINORE DI 12000
266900990907    2C   25VI2VLM        IFLT      §GELRP
267000930219     C                   SETON                                        5699
267100930219     C                   GOTO      ENDCON
267200941111    2C                   END
267300921009     C*
267400920827     C*** TIPO VALORE MERCE
267500920827     C* TIPO VALORE OBBLIGATORIO
267600941111    2C     VI2FVM        IFEQ      '?'
267700980316     C                   MOVEL     'TR'          COD
267800920925     C                   MOVEL     ' '           VI2FVM
267900001020     C                   MOVE      *BLANKS       KEY
268000920925     C                   MOVEL     *BLANKS       DES
268100920925     C                   CALL      'X§TABER'
268200920925     C                   PARM                    CODUT
268300920925     C                   PARM                    COD
268400920925     C                   PARM                    KEY
268500920925     C                   PARM                    DES
268600920925     C                   MOVEL     KEY           VI2FVM
268700920925     C                   MOVEL     DES           DESFVM
268800920925     C                   SETON                                        98
268900920925     C                   GOTO      ENDCON
269000941111    2C                   END
269100920925     C*
269200941111    2C     VI2FVM        IFEQ      ' '
269300920827     C                   SETON                                        5799
269400920827     C                   GOTO      ENDCON
269500941111    2C                   END
269600941111     C*
269700920827     C* CONTROLLO VALIDITA' TIPO VALORE
269800980316     C                   MOVEL     'TR'          COD
269900920827     C                   MOVEL     *BLANKS       KEY
270000920827     C                   MOVEL     VI2FVM        KEY
270100920827     C     KTAB          CHAIN     TABEL                              34
270200920827     C*
270300920827     C* SE TIPO VALORE ANNULLATO : ERRORE
270400941111    2C  N34TBLFLG        IFNE      ' '
270500920827     C                   SETON                                        5899
270600920827     C                   GOTO      ENDCON
270700941111   X2C                   ELSE
270800980316     C                   MOVEL     TBLUNI        DSTR
270900941111     C* CONTROLLO SE E' DA UTILIZZARE PER IL CALCOLO VALORE
271000980316     C     §TRUCV        IFNE      'S'
271100920827     C                   SETON                                        5999
271200920827     C                   GOTO      ENDCON
271300920827     C                   ELSE
271400920827     C* DECODIFICO TIPO VALORE
271500980316     C                   MOVEL     §TRDES        DESFVM
271600920827     C                   END
271700941111    2C                   END
271800920827     C*
271900920827     C* 34 ON - TIPO VALORE MERCE INESISTENTE
272000920827     C   34              SETON                                        5999
272100060802     C                   GOTO      ENDCON
272200941111    1C                   END
272300140219      * FUEL
272400060802      * 82 ON - controllo la data riferimento prezzo base
272500060802     c                   if        *in82
272600140219      * il prezzo non può essere a zero
272700140219     c                   if        vi2pmg = 0
272800140219     c                   eval      *in83 = *on
272900140219     c                   eval      *in99 = *on
273000060802     c                   goto      endcon
273100060802     c                   endif
273200140219      * verifico la correttezza del prezzo richiamndo il TRULPMG
273300140219     c                   clear                   trulpmgds
273400140219     c                   eval      ipmgpmg = vi2pmg
273500140219     c                   clear                   kpjbu
273600140219     c                   eval      kpjbu = Trulpmgds
273700140219     c                   call      'TRULPMGR'
273800140219     c                   parm                    kpjba
273900140219     c                   eval      trulpmgds = kpjbu
274000140219      * scaglione non trovato
274100140219     c                   If        opmgerr = '1'
274200140219     c                   eval      *in84 = *on
274300140219     c                   eval      *in99 = *on
274400140219     c                   eval      vo2sca = *zeros
274500150217     c                   eval      vo2dpb = *zeros
274600140219     c                   goto      endcon
274700140219     c                   endif
274800140219      * prezzo mai rilevato settimanalmente dal Ministero
274900140219     c                   If        opmgerr = '2'
275000140219     c                   eval      *in85 = *on
275100140219     c                   eval      *in99 = *on
275200140219     c                   eval      vo2sca = *zeros
275300150217     c                   eval      vo2dpb = *zeros
275400140219     c                   goto      endcon
275500140219     c                   endif
275600140219     c                   eval      vo2sca = opmgsca
275700150217     c                   eval      vo2dpb = opmgdtd
275800140219     c                   eval      vi2pmg = opmgpmg
275900060802
276000060802     c                   endif
276100920827     C*
276200920611     C     ENDCON        ENDSR
276300921009     C*
276400920624     C*--- AGGIORNO CAMPI DEL SFL ------------------------------------*
276500920610     C     REGIS         BEGSR
276600920709     C*
276700920610     C                   Z-ADD     1             NR2
276800941111     C     NR2           CHAIN     TA67S07                            31
276900920610     C     *IN31         DOWEQ     '0'
277000920616     C     VI2CTS        IFNE      *BLANKS
277100921126     C*
277200921126     C                   SETOFF                                       60
277300921126     C* VI2NRR = 0  SI TRATTA DI RECORD DI NUOVA IMMISSIONE
277400921126     C     VI2NRR        IFEQ      0
277500921216     C                   SETON                                        60
277600921126     C*
277700921126     C                   ELSE
277800921126     C     VI2ATB        IFEQ      ' '
277900921126     C     VI2REC        IFNE      SAVREC
278000921216     C                   SETON                                        60
278100921126     C                   END
278200921126     C                   ELSE
278300921126     C                   SETON                                        60
278400921126     C                   END
278500921126     C                   END
278600921126     C*
278700061011     C   60
278800061011     Cann12KTPD          CHAIN     TITPD000                           33
278900061011     c   60
279000061011     can 12ktpd          chain     tiopd000                           33
279100921126     C   60VI2ATB        IFNE      ' '
279200030715     c                   if        *in33 = *off
279300030715      * non è offerta
279400030715     c                   if        *in12 = *off
279500030715     C                   MOVEL     'A'           TPDATB
279600030715     C                   MOVEL     ' '           TPDFTR
279700040915     C                   Z-ADD     dateu8        TPDDTR
279800030715     C                   UPDATE    TITPD000
279900030716      * verifico prima se esiste l'immissioine di questa tariffa
280000030716     c                   movel(p)  VIDftc        w_manftc
280100030716     c                   move      'I'           W_manftc
280200030716     c                   z-add     1             WW
280300030716     c     w_manftc      lookup    manftc                                 87
280400030715      * mi salvo la tariffa particolare da registrare nello storico variazioni
280500030716      * se non esiste la fase di immissione
280600030716     c                   if        not %found
280700030716      *
280800030715     c                   movel(p)  VIDftc        w_manftc
280900030715     c                   move      'M'           W_manftc
281000030715     c                   z-add     1             WW
281100030715     c     w_manftc      lookup    manftc                                 87
281200030715     c                   if        not %found
281300030715     c     '   '         lookup    manftc(ww)                             87
281400030715     c                   if        %found
281500030715     c                   movea     w_manftc      manftc(ww)
281600030715     c                   endif
281700030715     c                   endif
281800030716      *
281900030716     c                   endif
282000030715      *
282100030715     c                   else
282200030715      * offerta
282300061011     C                   DELETE    tiopd000
282400030715     c                   endif
282500921218     C*
282600030715     c                   endif
282700030715     C*
282800990615     C* AGGANCIO TNTPT PER AGGIORNARE ULTIMA DATA VARIAZIONE CON TRASMISSIONE
282900990615     C* DEL DATO
283000061011     C  n12KTPT          CHAIN     TITPT000                           32
283100061011     c   12ktpt          chain     tiopt000                           32
283200990615     C  N32              CLEAR                   TPTFTR
283300990615     C* DATA ULTIMA VARIAZIONE
283400990615     C  N32              Z-ADD     DATEU8        TPTDUV
283500040915     C  N32              z-add     dateu8        TPTDTR
283600061011     C  N32
283700061011     Cann12              UPDATE    TITPT000
283800061011     c  n32
283900061011     can 12              update    tiopt000
284000990615     C*
284100921218     C*
284200920610     C                   ELSE
284300920611     C* 33 ON  - IMMISSIONE
284400920611     C* 33 OFF - VARIAZIONE
284500941110     C                   Z-ADD     VI2ITR        TPDITR
284600940712     C                   Z-ADD     VI2MIN        TPDMIN
284700940713     C                   Z-ADD     VI2MAX        TPDMAX
284800930217     C*
284900930219     C   06VI2AIN        IFEQ      'N'
285000930219     C                   MOVEL     ' '           TPDAIN
285100930219     C                   ELSE
285200930219     C                   MOVEL     VI2AIN        TPDAIN
285300930219     C                   END
285400930217     C*
285500920611     C                   MOVEL     VI2ATB        TPDATB
285600920617     C                   MOVEL     VI2ORP        TPDORP
285700930217     C  N06              MOVEL     ' '           TPDAIN
285800921214     C                   MOVEL     ' '           TPDFTR
285900920611     C* SOLO PER IMMISSIONE
286000920611     C   33              DO
286100920611     C                   MOVEL     VI2CTS        TPDCTS
286200940712     C                   Z-ADD     VI2SGL        TPDSGL
286300920612     C                   Z-ADD     VIDCLI        TPDKSC
286400920612     C                   Z-ADD     VIDCTR        TPDCTR
286500920612     C                   Z-ADD     VIDPRG        TPDPRG
286600920612     C                   MOVEL     VIDFTC        TPDFTC
286700920610     C                   END
286800980220     C* VALORE POSITIVO NEI CAMPI
286900980220     C                   MLLZO     1             TPDSGL
287000980220     C                   MLLZO     1             TPDITR
287100980220     C                   MLLZO     1             TPDMIN
287200980220     C                   MLLZO     1             TPDMAX
287300920610     C*
287400040915     C                   Z-ADD     dateu8        TPDDTR
287500061011     c                   if        not *in12
287600061011     C  N33              UPDATE    TITPD000
287700061011     C   33              WRITE     TITPD000
287800061011     c                   endif
287900061011     c                   if        *in12
288000061011     c  n33              update    tiopd000
288100061011     c   33              write     tiopd000
288200061011     c                   endif
288300030715      *
288400921217     C*
288500990615     C* AGGANCIO TNTPT PER AGGIORNARE ULTIMA DATA VARIAZIONE CON TRASMISSIONE
288600990615     C* DEL DATO
288700061011     C  n12KTPT          CHAIN     TITPT000                           32
288800061011     c   12ktpt          chain     tiopt000                           32
288900030715     c                   if        *in32 = *off
289000030715     C                   CLEAR                   TPTFTR
289100040915     C                   Z-ADD     DATEU8        TPTDTR
289200990615     C* DATA ULTIMA VARIAZIONE
289300030715     C                   Z-ADD     DATEU8        TPTDUV
289400061011     C  n12              UPDATE    TITPT000
289500061011     c   12              update    tiopt000
289600030715      * mi salvo la tariffa particolare da registrare nello storico variazioni
289700030715      * se non sono in offerta
289800030715     c                   if        *in12 = *off
289900030715      *
290000030715     c                   movel(p)  VIDftc        w_manftc
290100030715     c                   move      'M'           W_manftc
290200030715     c                   z-add     1             WW
290300030715     c     w_manftc      lookup    manftc                                 87
290400030715     c                   if        not %found
290500030715     c     '   '         lookup    manftc(ww)                             87
290600030715     c                   if        %found
290700030715     c                   movea     w_manftc      manftc(ww)
290800030715     c                   endif
290900030715     c                   endif
291000030715     C*
291100030715     c                   endif
291200990615     C*
291300030715     c                   endif
291400921126     C*
291500920610     C                   END
291600920616     C                   END
291700920616     C                   ADD       1             NR2
291800941111     C     NR2           CHAIN     TA67S07                            31
291900920610     C                   END
292000920610     C                   ENDSR
292100920610     C*
292200920624     C*--- AGGIORNO CAMPI DEL SFLCTL ---------------------------------*
292300920624     C     REGIS2        BEGSR
292400920709     C*
292500920624     C* 32 ON  - IMMISSIONE
292600920624     C* 32 OFF - VARIAZIONE
292700061011     C  n12KTPT          CHAIN     TITPT000                           32
292800061011     c   12ktpt          chain     tiopt000                           32
292900920624     C                   MOVEL     VI2TPG        TPTTPG
293000940712     C                   Z-ADD     VI2ARL        TPTARL
293100920624     C                   Z-ADD     VI2ARF        TPTARF
293200920624     C                   Z-ADD     VI2ARO        TPTARO
293300920624     C                   Z-ADD     VI2RPV        TPTRPV
293400920709     C                   MOVEL     ' '           TPTATB
293500990915     C                   MOVEL     VI2MAS        TPTTMA
293600060802     c                   If        *in82
293700150217     C                   Z-ADD     vo2dpb        TPTDPB
293800140115     c                   eval      §TPTpma = VI2pma
293900060829     c                   else
294000060829     c                   clear                   tptdpb
294100140115     c                   clear                   §TPTpma
294200060803     c                   endif
294300150217      *
294400150219     c                   if        *in90
294500150219     c                   eval      §TPTvma = VO2vma
294600150219     c                   else
294700150219     c                   clear                   §TPTvma
294800150219     c                   endif
294900060803      *
295000060803     c   32              z-add     dateu8        TPTdit
295100941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
295200941111    1C     *IN11         IFEQ      *ON
295300941111     C                   Z-ADD     VI2VLM        TPTVLM
295400941111     C                   MOVEL     VI2FVM        TPTFVM
295500941111   X1C                   ELSE
295600941111     C                   Z-ADD     0             TPTVLM
295700941111     C                   MOVEL     ' '           TPTFVM
295800941111    1C                   ENDIF
295900941111     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
296000941111    1C     *IN28         IFEQ      *ON
296100941111     C                   MOVEL     VI2APL        TPTAPL
296200941111   X1C                   ELSE
296300941111     C                   MOVEL     ' '           TPTAPL
296400941111    1C                   ENDIF
296500941111     C*
296600920624     C* SOLO PER IMMISSIONE
296700941111    1C   32              DO
296800920624     C                   Z-ADD     VIDCLI        TPTKSC
296900920624     C                   Z-ADD     VIDCTR        TPTCTR
297000920624     C                   Z-ADD     VIDPRG        TPTPRG
297100920624     C                   MOVEL     VIDFTC        TPTFTC
297200941111    1C                   END
297300941111     C*
297400990614     C* SE VIENE VARIATO TITPT ABBLENCO FLAG E DATA TRASMISSIONE
297500941111    1C     COMTPT        IFNE      TPTDS
297600921215     C                   MOVEL     ' '           TPTFTR
297700040915     C                   Z-ADD     dateu8        TPTDTR
297800921217     C*
297900990903     C*
298000990615     C* DATA ULTIMA VARIAZIONE
298100990615     C                   Z-ADD     DATEU8        TPTDUV
298200941111    1C                   END
298300980220     C* VALORE POSITIVO NEI CAMPI
298400980220     C                   MLLZO     1             TPTARL
298500980220     C                   MLLZO     1             TPTARF
298600980220     C                   MLLZO     1             TPTARO
298700980220     C                   MLLZO     1             TPTRPV
298800980220     C                   MLLZO     1             TPTVLM
298900921215     C*
299000040915     C                   Z-ADD     dateu8        TPTDTR
299100140115     c                   eval      TPTflo = dTPT01
299200061011     c                   if        not *in12
299300990614     C  N32              UPDATE    TITPT000
299400990614     C   32              WRITE     TITPT000
299500061011     c                   endif
299600061011     c                   if        *in12
299700061011     c  n32              update    tiopt000
299800061011     c   32              write     tiopt000
299900061011     c                   endif
300000030715      *
300100030715      * se non sono in offerta salvo il codice tariffa particolare  da scrivere
300200030715      * nello storico
300300030715      *
300400030715     c                   IF        *in12 = *off
300500030715      *
300600030715     c                   movel(p)  VIDftc        w_manftc
300700030715     c  n32              move      'M'           W_manftc
300800030715     c   32              move      'I'           W_manftc
300900030715     c                   z-add     1             WW
301000030715     c     w_manftc      lookup    manftc                                 87
301100030715     c                   if        not %found
301200030715     c     '   '         lookup    manftc(ww)                             87
301300030715     c                   if        %found
301400030715     c                   movea     w_manftc      manftc(ww)
301500030715     c                   endif
301600030715     c                   endif
301700921126     C*
301800030715     c                   endif
301900030715     c*
302000930318     C*
302100930318     C                   SETON                                        0207
302200930318     C                   SETOFF                                       01
302300930223     C*
302400930223     C                   MOVEL     VI2TPG        SAVTPG            1
302500930223     C                   Z-ADD     VI2VLM        SAVVLM            9 0
302600920624     C                   ENDSR
302700131218     C*
302800131218     C*--- Recupero prezzo del gasolio corrispondente ----------------*
302900131218     C     REC_pmg       BEGSR
303000131218     C*
303100131218     c     Kpmg          setll     tipmg01l
303200131218     c                   read      tipmg01l
303300131218     c                   if        %found(tipmg01l)
303400131218      * devo controllare la data del prezzo medio che ho trovato
303500131218      * se la data del prezzo medio è troppo indietro non devo visualizzare
303600131218      * il prezzo (per ora faccio il controllo con 6 gg)
303700131218     c                   if        pmgdtd > *zeros
303800131218     c     *iso          move      pmgdtd        data_iso1
303900131218     c                   else
304000131218     c                   eval      data_iso1 = *loval
304100131218     c                   endif
304200131218     c     *iso          move      Kpmg          data_iso2
304300131218     c     data_iso2     subdur    data_iso1     wgio:*d
304400131218     c                   if        wgio > 6
304500140219     c                   clear                   vI2pmg
304600140219     c                   clear                   vO2sca
304700131218     c                   else
304800140219     c                   eval      vI2pmg = pmgpmg
304900140219     c                   eval      vO2sca = pmgsca
305000131218     c                   endif
305100131218     c                   endif
305200131218     C                   ENDSR
305300131218     C*
305400120320
305500120320      /free
305600120320       //--------------------------------------------------------------
305700120320       //?Visualizza Dati manca tariffa-
305800120320       //--------------------------------------------------------------
305900120320       BEGSR GesW01;
306000120320
306100120320         wFine = *off;
306200120320         W01dsp = ISB48dsp;
306300120320         W01tbl = ISB48tbl;
306400120320         W01lnp = ISB48lnp;
306500120320         W01lna = ISB48lna;
306600120320         W01pro = ISB48pro;
306700120320         W01cts = ISB48cts;
306800120320         W01ctr = ISB48ctr;
306900120320         W01ncl = ISB48ncl;
307000120320         W01pkf = ISB48pkf;
307100120320         W01vlf = ISB48vlf;
307200120320         W01err = ISB48err;
307300120320
307400120320         DOW wFine = *off;
307500120320           exfmt  TA67W01;
307600120320           //?- F12 = Ritorno
307700120320           IF  *inkl;
307800120320             wFine = *on;
307900120320           ENDIF;
308000120320         ENDDO;
308100120320
308200120320       ENDSR;
308300120320      /end-free
308400950119     C*---------------------------------------------------------------*
308500020207**  MS1
308600950119E' possibile inserire solo il Codice Tassazione "xx"                           1
308700950119**  MS2
308800950119E' possibile inserire solo i Codici Tassazione "xx" e "xx"                     1
308900011022**  MSG
309000050531Non è possibile inserire il Codice Tassazione "EE"                             1
309100050531Codice tassazione non utlizzabile per tariffa FedEx                            2
309200050531Codice tassazione utilizzabile solo per tariffe FedEx                          3
309300050531Non è possibile inserire il Codice Tassazione "IT"                             4
309400050531Impossibile inserire la Tar.Particolare "XX" che sarà eliminata il xx/xx/xx    5
309500050704Inserire solo uno scaglione per codice di tassazione!!                         6
309600140128Cliente " " l'importo non può essere inferiore a Euro                          7
309700060307Impossibile inserire AC BASE. Il cliente ha rinunciato al mandato ass. AC BASE 8
309800060307Impossibile inserire AC BASE. Presente AC PLUS reale con applicazione completa 9
309900060307Impossibile inserire AC PLUS reale con applicazione completa. Presente AC BASE 1
310000060705L'importo non può essere inferiore a xxxxxxxx,xxx Euro                        11
310100070226Codice tassazione non utilizzabile                                            12
310200140219                                                                              13
