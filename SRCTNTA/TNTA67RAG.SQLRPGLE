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
004500141118     d sgl             s             13  3 dim(18)
004600141118     d itr             s             11  3 dim(18)
004700141118
004800030715     d MANftc          s              3    dim(150)
004900030715     d W_manftc        s              3
005000030715     d WW              s              3  0 inz
005100051012     d Kscfil          s              3  0
005200080609
005300080609     d xx              s              3  0
005400131218
005500131218     d wgio            s              5  0
005600060801
005700060801     D Wdataeur        S               D   DATFMT(*eur)
005800060801     D Wdataoggi       S               D   DATFMT(*eur)
005900060802     d Datamax         s              8  0
006000131218     d data_iso1       s               d   datfmt(*iso)
006100131218     d data_iso2       s               d   datfmt(*iso)
006200030715
006300021209     d Wfqt            s                   Like(§1pFqt)
006400050531     d
006500050531     d wdatadmy        s               d   datfmt(*dmy)
006600050704
006700050704     d savcts          s                   like(tpdcts)
006800061011     d codut           s              1  0 Inz(1)
006900060306     d kcc             s                   like(clpkcc) inz(151)
007000060306     d ksc             s                   like(clpksc)
007100021209
007200131218     d Kpmg            s                   like(TPTdpb)
007300120320
007400120320       // -?Flag booleani
007500120320     d wFine           s               n   inz(*off)
007600130125     d wBlocca         s               n   inz(*off)
007700060802
007800941020     D*
007900920612     D PARAM5          DS
008000941111     D* CODICE CLIENTE/NUMERO VISITA
008100920612     D  VIDCLI                 2      8  0
008200941111     D* RAGIONE SOCIALE
008300920612     D  RAGDES                 9     56
008400941111     D* PROGRESSIVO TARIFFA
008500920612     D  VIDPRG                57     59  0
008600941111     D* CODICE TARIFFA
008700920612     D  VIDCTR                60     62  0
008800941111     D* DECODIFICA CODICE TARIFFA
008900920612     D  VIDDCF                63     72
009000941111     D* DESCRIZIONE TARIFFA
009100920612     D  VIDDCV                73     87
009200941111     D* TESTATA PROGRAMMA
009300920612     D  VIDTES                88    121
009400941111     D* FLG = " " ---> TARIFFE     FLG <> " " ---> OFFERTE
009500920619     D  FLG                  122    122
009600941111     D* FLG2   --> SE RESTITUITO PIENO INDICA CHE E' STATO PREMUTO CMD3
009700921117     D  FLG2                 123    123
009800980309     D* FLAG ITALIA/ESTERO
009900980309     D  PARFIE               125    125
010000980429     D* FLAG TARIFFA IMPORT
010100980429     D  IMPORT               126    126
010200000420     D* FLAG CLIENTE POSTA
010300000420     D  WRKPT                127    127
010400990824     D* CODICE DELLA  DIVISA
010500990824     D  VICDIV               128    130
010600990622     D* FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
010700990824     D  FLGDEC               131    131
010800000915     D* DATA DECORRENZA TARIFFA
010900000915     D  PARDDT               132    139  0
011000000915     D* TIPO SEWRVIZIO BOLLA
011100000915     D  PARTSP               140    140
011200020201     D* Flag Network
011300020201     D  FLGNET               141    141
011400050531     D* DATA scadenza TaRIFFA alfanumerica
011500050531     D  PARDST               142    149
011600060306      * flag rinuncia AC Base
011700060306     d  parfmp               150    150
011800090916      * flag per richiamo da trattativa = 'T'
011900090916     d  partrat              151    151
012000941111     D*
012100920616     D                 DS
012200940712     D  VI2SGL                 1     13  3
012300941110     D  VI2ITR                14     24  3
012400990614     D  VI2MIN                25     35  3
012500990614     D  VI2MAX                36     46  3
012600990614     D  RECORD                 1     46
012700990614     D  VI2AIN                47     47
012800990614     D  VI2REC                14     47
012900921126     D                 DS
013000941110     D  SAVITR                 1     11  3
013100990614     D  SAVMIN                12     22  3
013200990614     D  SAVMAX                23     33  3
013300990614     D  SAVAIN                34     34
013400990614     D  SAVREC                 1     34
013500990615     D WLBDAT          DS
013600060802     D  G08DAT                 1      8  0
013700060802     D  G08INV                 9     16  0
013800060802     D  G08ERR                17     17
013900060802     D  G08TGI                18     22  0
014000920612     D KPJBA         E DS
014100920610     D TAR             DS
014200920610     D  TPDNRR               397    400B 0
014300061011     d taro            ds
014400061011     d  tponrr               397    400B 0
014500990614     D TPTDS         E DS                  EXTNAME(TITPT00F)
014600921215     D DS1P          E DS
014700020201     D DSSP          E DS
014800990907     D DGEI          E DS
014900920612     D DSCT          E DS
015000940913     D DSTR          E DS
015100950112     D DS15          E DS
015200060705     d dfab          e ds
015300140219      *
015400990217     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015500030718     D TNTA04DS      E DS
015600061011
015700061011     d Azuteds       e ds                  extname(Azute00f)
015800061011     d dDatiute      e ds
015900061011     d Tibs34ds      e ds
016000130125     d dUTE01        e ds
016100061011
016200061011     d Tnta25ds      e ds
016300061011     d Tnta25tad     e ds                  inz
016400061011     d Tnta25tgc     e ds                  inz
016500061011     d Tnta25tpd     e ds                  inz
016600061011     d Tnta25tpt     e ds                  inz
016700080609
016800080609     d dlav          e ds
016900080609     d tntbeds       e ds                  extname(tntbe00f)
017000120320
017100120320       // -?ds dati passati da TNSB48R - Manca Tariffa
017200120320     d TNSB48ds      e ds
017300120320     d Parm48          s                   like(tnsb48ds)
017400120713
017500120713       // -?Passaggio dati a TRTB09R - Ricerca codici tassazione
017600120713     d TRTB09DS      e ds
017700140115
017800140219       // -?Passaggio dati a TRULPMGR - Controllo Prezzo base di riferimento
017900140219     d TRULPMGDS     e ds
018000140219
018100140220       // -?Interrogazione e recupero Prezzi medi del Gasolio
018200140220     d TNTA39DS      e ds
018300140220
018400141118       // -?Richiamo al programma di calcolo Fuel
018500141118     d TNTA42DS      e ds
018600141118
018700140115       // -?DS campo TPTflo
018800140115     d dTPT01        e ds
018900140115
019000140115       // -?Costante per controllo campi numerici
019100140115     d c_Digits        c                   const('0123456789')
019200090916
019300090916     itivis000
019400090916     i              visesi                      wisesi
019500090916
019600920529     C*****************************************************************
019700920529     C* RIEPILOGO INDICATORI
019800920529     C*****************************************************************
019900920609     C* 01    - IMMISSIONE
020000920609     C* 02    - VARIAZIONE
020100920618     C* 03    - USATO IN VARIAZIONE PER POSIZIONARMI SU CODICE TASSAZ.
020200920618     C* 04    - USATO IN VARIAZIONE PER POSIZIONARMI SU IMPORTO TARIFFA
020300920609     C* 05    - USATO IN VARIAZIONE PER PROTEGGERE I CAMPI
020400920609     C* 06    - TIPO CONSEGNA = ISOLE MINORI
020500920619     C* 07    - PREMUTO CMD6-AGGIORNAMENTO
020600920624     C* 08    - ROLLUP
020700920618     C* 09    - PREMUTO CMD16-ANNULLAMENTO
020800920707     C* 10    - USATO IN INTERROGAZIONE PER PROTEGGERE I CAMPI
020900941111     C* 11    - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
021000920619     C* 12    - PROVENGO DA OFFERTA
021100950119     C* 13    - INDICATORE DI EMISSIONE CAMPO MESSAGGIO DI ERRORE
021200941110     C* 14    - INDICA TIPO TARIFFA/TIPO PAGAMENTO A VALORE
021300920616     C* 15/19 - TASTI DI DUPLICA
021400920707     C* 20    - LOKUP
021500981016     C* 21    - ON  PER IL TIPO CONSEGNA  RCV
021600981016     C*         PROTEGGO IL CAMPO TIPO VALORE.DEVE ESSERE SEMPRE A KG
021700920625     C* 22    - USATO IN ASSICURAZIONE X CONTO PER PROTEGGERE CAMPI
021800920625     C*         DEL FORMATO
021900941111     C* 23    - TASTO DI DUPLICA
022000990217     C* 24    - TARIFFE PARTIC. ASSICURAZIONE X CONTO
022100990217     C* 25    - TAIRFFE PARTIC. RCV
022200920827     C* 26    - USATO X CONDIZIONARE L'EMISSIONE DEGLI ARROTONDAMENTI
022300941110     C* 27    - USATO X CONDIZIONARE L'EMISSIONE DEL   RAPPORTO P/VOL.
022400941111     C* 28    - VISUALIZZO CODICE APPLICAZIONE
022500921007     C* 29    - USATO PER AZZERARE I CAMPI DEL SFLCONTROL NEL CASO IN
022600941110     C*         CUI VENGA VARIATO IL TIPO TARIFFA/TIPO PAGAMENTO
022700920609     C* 30/34 - DI COMODO
022800920609     C* 35/37 - PULIZIA SUBFILE
022900920609     C* 36    - SFLNXTCHG
023000941116     C* 38    - VISUALIZZO IL MINIMO  TARIFFA
023100941116     C* 39    - VISUALIZZO IL MASSIMO TARIFFA
023200941111     C* 40/42 - ERRORE
023300980514     C* 43    - VISUALIZZO IL FLAG MANDATO ASSICURATIVO FITTIZIO
023400990217     C*         SOLO EPR "C" -ASS/C
023500000918     C* 44    - SI TRATTA DI TARUFFA DI CARTELLO PROTEGGO LA COPIA TARIFFE DI
023600000918     C*         CARTELLO
023700941111     C* 45/51 - ERRORE
023800980429     C* 52/59 - ERRORE
023900921126     C* 60    - ON  SIGNIFICA CHE DEVO REGISTRARE IL RECORD
024000941111     C* 61    - USATO PER EMETTERE IL MESSAGGIO DEL CMD14 DI FORZATURA
024100031003     C* 62    - ON SIGNIFICA CHE NON C'E' DETTAGLIO TARIFFE PARTICOLARI
024200990622     C* 63/65 - ERRORE
024300990622     C* 66/73 - ERRORE
024400950119     C* 74    - POSIZIONAMENTO CURSORE SUL CODICE TASSAZIONE PER
024500131217      * 75    - Errore data prezzo base FUEL
024600950119     C*         EMISSIONE CAMPO MESSAGGIO
024700060303     C* 76/77 - ERRORE VI2CTS
024800060303      * 78    - VARIA "d" AC BASE
024900060306      * 79    - Posizionamento cursore
025000000829     C* 80/81 - PULIZIA SUBFILE
025100140219      * 82/85 - Tariffa Particolare "f" Supplemento carburante
025200140219      * 86    - Posizione Cursore Tariffa Particolare "f" Suppl.carbur.
025300140131      * 88    - Tariffa particolare "f" deve avere scaglione >= 999
025400090916      * 89    - Trattativa
025500140219      * 90    - Utente EDPGA
025600120320      * 97    - Pgm richiamato x manca tariffa TNSB48
025700920611     C* 98    - DI COMODO
025800920609     C* 99    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
025900920529     C*****************************************************************
026000920529     C     *ENTRY        PLIST
026100920529     C                   PARM                    KPJBA
026200030715     C                   PARM                    manftc
026300120320     c                   parm                    Parm48
026400920615     C                   MOVEL     KPJBU         PARAM5
026500140219     c                   eval      *in90 = (Knmus = 'EDPGA')
026600140219     c
026700080609
026800080609     c/exec sql
026900080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
027000080609     c/end-exec
027100061011
027200061011     c     *dtaara       define    §azute        azuteds
027300061011     c     *dtaara       define    §datiute      ddatiute
027400061011     c                   in(E)     *dtaara
027500061011     c                   If        %error  or RSUT = *blanks
027600061011     c                   Clear                   Tibs34ds
027700061011     c                   Call      'TIBS34R'
027800061011     c                   Parm                    Tibs34ds
027900061011     c                   In        *dtaara
028000061011     c                   EndIf
028100120320
028200120320
028300120320      /free
028400120320       //?controllo se passati tutti i parametri
028500120320         IF  %parms < 3;
028600120320           clear tnsb48ds;
028700120320           *in97 = *off;
028800120320         ELSE;
028900120320           clear tnsb48ds;
029000120320           tnsb48ds = parm48;
029100120320           *in97 = *on;
029200120320         ENDIF;
029300120320
029400120320         clear OSB48err;
029500120320         clear OSB48msg;
029600120320      /end-free
029700120320
029800120320
029900990615     C* PRENDO LA DATA DEL GIORNO DA TIME
030000990615     C                   TIME                    W0140            14 0
030100990615     C                   MOVE      W0140         UDATE8            8 0
030200990615     C*
030300990615     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
030400990615     C*
030500060802     C                   Z-ADD     UDATE8        G08DAT
030600060802     C                   MOVEL     *BLANK        G08ERR
030700990615     C                   CALL      'XSRDA8'
030800990615     C                   PARM                    WLBDAT
030900990615     C* UDATE A 8 IN AAAA/MM/GG
031000060802     C                   Z-ADD     G08INV        DATEU8            8 0
031100060801     C     *iso          movel     dateu8        Wdataoggi
031200060801      * calcolo la data massima di controllo per supplemento carburante
031300060803     c     Wdataoggi     adddur    06:*m         Wdataeur
031400060802     c     *iso          movel     Wdataeur      Datamax
031500090916
031600090916      * controllo se richiamato da trattativa
031700090916     c                   if        partrat = 'T'
031800090916     c                   eval      *in89 = *on
031900090916     c                   else
032000090916     c                   eval      *in89 = *off
032100090916     c                   endif
032200920529     C*---------------------------------------------------------------*
032300990614     C* ACCESSO TITPT01L/TITPD01L
032400920612     C     KTPT          KLIST
032500920612     C                   KFLD                    VIDCLI
032600920612     C                   KFLD                    VIDCTR
032700920612     C                   KFLD                    VIDPRG
032800920612     C                   KFLD                    VIDFTC
032900000901     C* ACCESSO A TPT/TPD DA SFL COPIA DA CARTELLO
033000000901     C     KTPT9         KLIST
033100000901     C                   KFLD                    VIDCLI
033200000901     C                   KFLD                    VIDCTR
033300000901     C                   KFLD                    VIDPRG
033400000901     C                   KFLD                    V09FTC
033500990614     C* ACCESSO TITPD01L
033600920612     C     KTPD          KLIST
033700920612     C                   KFLD                    VIDCLI
033800920612     C                   KFLD                    VIDCTR
033900920612     C                   KFLD                    VIDPRG
034000920612     C                   KFLD                    VIDFTC
034100920612     C                   KFLD                    VI2ORP
034200940712     C                   KFLD                    VI2SGL
034300050704      *
034400050704     c     ktpd01        klist
034500050704     c                   kfld                    vidcli
034600050704     c                   kfld                    vidctr
034700050704     c                   kfld                    vidprg
034800050704     c                   kfld                    vidftc
034900050704     c                   kfld                    vi2orp
035000940712     C* ACCESSO TABEL00F
035100920608     C     KTAB          KLIST
035200920608     C                   KFLD                    CODUT
035300940712     C                   KFLD                    COD
035400940712     C                   KFLD                    KEY
035500920608     C     KTAB2         KLIST
035600920608     C                   KFLD                    CODUT
035700920608     C                   KFLD                    COD
035800060306
035900060306      * cnclp00f/tfclp00f
036000060306     c     kclp          klist
036100060306     c                   kfld                    codut
036200060306     c                   kfld                    kcc
036300060306     c                   kfld                    ksc
036400140128
036500140128      * TIACB01L
036600140128     c     kACB          klist
036700140128     c                   kfld                    CLPclv
036800140128     c                   kfld                    dateu8
036900140128
037000940712     C***
037100940712     C** DEFINIZIONE CAMPI
037200940712     C***
037300940712     C     *LIKE         DEFINE    TBLCOD        COD
037400940712     C     *LIKE         DEFINE    TBLKEY        KEY
037500941109     C     *LIKE         DEFINE    §1PCTS        WCTS
037600941123     C     *LIKE         DEFINE    §1PCTS        WCTE
037700941109     C     *LIKE         DEFINE    §1PPTV        WPTV
037800940712     C* CAMPI DI DUPLICA
037900940712     C     *LIKE         DEFINE    TPDCTS        DUPCTS
038000940712     C     *LIKE         DEFINE    TPDSGL        DUPSGL
038100941110     C     *LIKE         DEFINE    TPDITR        DUPITR
038200940712     C     *LIKE         DEFINE    TPDMIN        DUPMIN
038300940712     C     *LIKE         DEFINE    TPDMAX        DUPMAX
038400940712     C     *LIKE         DEFINE    TPDAIN        DUPAIN
038500940712     C*
038600920529     C*---------------------------------------------------------------*
038700930223     C***** REPERISCO IL LIMITE RISARCIBILE STABILITO PER LEGGE  *****
038800990907     C*****   DALLA TABELLA GEN PASSANDOGLI LA MONETA            *****
038900990907      *
039000990907     C                   CLEAR                   DSBS02
039100990907     C                   CLEAR                   DGEI
039200990907     C                   MOVEL     'C'           T02MOD
039300990907     C                   MOVEL     KNSIF         T02SIF
039400990907     C                   MOVEL     'GEI'         T02COD
039500990907     C                   MOVEL     VICDIV        T02KE1
039600990907      *
039700990907     C                   CALL      'TIBS02R'
039800990907     C                   PARM                    KPJBA
039900990907     C                   PARM                    DSBS02
040000990907      *
040100990907     C     T02ERR        IFEQ      *BLANKS
040200990907     C                   MOVEL     T02UNI        DGEI
040300990907     C                   ENDIF
040400920609     C*
040500980317     C* CARICO TABELLA CODICI NAZIONE
040600980317     C                   EXSR      CARNAZ
040700080609
040800080609      * carico tabella filiali attive al lasciato avviso
040900080609     c                   exsr      carlav
041000920619     C*
041100920619     C* CONTROLLO SE PROVENGO DA OFFERTA
041200920619     C     FLG           IFNE      ' '
041300920619     C                   Z-ADD     VIDCLI        VIDNRV
041400090916     C                   Z-ADD     VIDCLI        VIDNRVT
041500920619     C                   SETON                                        12
041600110103     c**n89              open      tnvis05l
041700090916     c   89              open      tivis05l
041800110103     c**n89vidnrv        chain     tnvis05l
041900090916     c   89vidnrv        chain     tivis05l
042000090916     c                   If        %found
042100051012     c                   movel     viscmm        kscfil
042200051012     c                   else
042300051012     c                   clear                   kscfil
042400051012     c                   endif
042500110103     c**n89              close     tnvis05l
042600090916     c   89              close     tivis05l
042700060306     c                   open      tfclp00f
042800111122     c                   open      cnclp00f
042900061011
043000061011     c                   open      tiopt01l
043100061011     c                   open      tiopd01l
043200061011
043300051012     c                   else
043400051012      * no offerta
043500051012     c                   movel     vidcli        kscfil
043600060306     c                   open      cnclp00f
043700061011     c                   open      titpt01l
043800061011     c                   open      titpd01l
043900061011     c                   setoff                                       12
044000920619     C                   END
044100990217     C**
044200040429      * verifico codice cliente se inizia con 88888 si tratta di CARTELLO
044300040429     c                   movel     vidcli        w0050             5 0
044400040429     c                   if        w0050 = 88888
044500040429     c                   seton                                        44
044600040429     c                   endif
044700060306      * se non è tariffa di cartello recupero il cod.importanza del CLIENTE
044800060306      * o della VISITA
044900060306     c                   if        not *in44
045000060306     c                   move      vidcli        ksc
045100060306     c  n12kclp          chain     cnclp00f
045200060306     c   12kclp          chain     tfclp00f
045300111122     c                   If        *in12 and not %found(tfclp00f)
045400111122      * se non trovata l'anagrafica provvisoria aggancio l'anagrafica del client
045500111122      * e della trattativa
045600111122     c                   move      visksc        Ksc
045700111122     c     kclp          chain     cnclp00f
045800111122     c                   endif
045900111121     c                   If        clpclv = ' '
046000111121     c                   eval      clpclv = 'C'
046100111121     c                   Endif
046200060306     c                   endif
046300130125
046400130125       //?Imposto flag x bloccare le tariffe particolari in manutenzione
046500130125       //?ma solo TARIFFE e NO CARTELLO
046600130125     c                   eval      dUTE01 = UTEfaf
046700130125     c                   IF        not *in12 and not *in44
046800130125     c                             and §UTEmodtar = *blanks
046900130125     c                   eval      wBlocca = *on
047000130125     c                   ENDIF
047100000829     C*
047200000829     C     CARTAG        TAG
047300921126     C*
047400920529     C* CARICO SUBFILE
047500920529     C                   EXSR      CARICA
047600920609     C* SE NON CARICA NIENTE ESCO
047700920529     C     NRR           IFEQ      0
047800920529     C                   GOTO      FINE
047900920529     C                   END
048000920529     C*
048100920529     C     INIZIO        TAG
048200940913     C                   WRITE     TA67T01
048300940913     C                   WRITE     TA67Z03
048400940913     C                   EXFMT     TA67C02
048500050531     c
048600050531     c                   setoff                                       4013
048700920529     C*
048800921117     C** CMD3  - FINE LAVORO
048900921117     C   KC              MOVEL     'A'           FLG2
049000921118     C   KC
049100921117     C** CMD12 - RITORNO
049200921117     COR KL              GOTO      FINE
049300030718     C** CMD07 - INTERROGAZIONE STORICO VARIAZIONI
049400030718     c                   if        *inkg
049500030718     c                   clear                   tnta04ds
049600030718     c                   eval      i04ksc = vidcli
049700030718     c                   eval      i04ctr = vidctr
049800030718     c                   eval      i04prg = vidprg
049900030718     c                   eval      i04rag = ragdes
050000030718     c                   eval      i04dct = viddcf
050100030718     c                   eval      i04dcv = viddcv
050200030718     c                   eval      i04div = vicdiv
050300030718     c                   eval      i04ndf = flgnet
050400030718      *
050500030718     c                   call      'TNTA04R'
050600030718     c                   parm                    tnta04ds
050700030718      *
050800030718     c                   goto      inizio
050900030718      *
051000030718     c                   endif
051100120320       //?F1 - dati manca tariffa
051200120320     c                   IF        *inka
051300120320     c                   exsr      gesw01
051400120320     c                   goto      inizio
051500120320     c                   ENDIF
051600030718     C*
051700000829     C** CMD13 - COPIA TARIFFE PARTICOLARI DA CARTELLO
051800000829     C     *INKM         IFEQ      *ON
051900000829     C                   EXSR      COPCAR
052000000829     C                   GOTO      CARTAG
052100000829     C                   ENDIF
052200000829     C**
052300920529     C* CONTROLLO DATI SUBFILE
052400920529     C                   SETOFF                                       31
052500940913     C                   READC     TA67S02                                31
052600921118     C   31              GOTO      FINE
052700050531    1C     *IN31         DOWEQ     '0'
052800980317     C*
052900980317     C                   SETOFF                                       0709
053000050531     c                   z-add     1             rec
053100920708     C*
053200050531    2C     VIDSCE        IFEQ      '1'
053300050531     c                   clear                   wfqt
053400050531     c* se tariffa non essitente, non la faccio inserire se in scadenza,
053500050531     c*  da eliminare
053600050531    3c                   if        vidflg=' ' and viheli>*zeros and
053700050531     c                             pardst>=viheli
053800050531     c                   eval      vidmsg=msg(5)
053900050531     c                   eval      %subst(vidmsg:42:2)=vidftc
054000050531     c                   movel     viheli        wdata8            8 0
054100050531     c     *iso          move      wdata8        wdatadmy
054200050531     c                   move      wdatadmy      wdata6            6 0
054300050531     c                   eval      %subst(vidmsg:68:8) =(%editw(wdata6:'  /  / -
054400050531     c                              '))
054500050531     c                   seton                                        13
054600050531    3c                   endif
054700060306
054800060306      * se immissione della tariffa particolare "d " AC BASE la tariffa non deve
054900060306      * avere il flag di rinuncia AC BASE
055000060306     c                   if        not *in13 and vidflg = *blanks and
055100060306     c                             vidftc = 'd ' and parfmp = 'S'
055200060306     c                   eval      vidmsg = msg(8)
055300060306     c                   eval      *in13 = *on
055400060306     c                   endif
055500060306
055600060306      * se immissione della tariffa particolare "d " AC BASE la tariffa non deve
055700060306      * avere una tariffa particolare AC PLUS reale con applicazione completa
055800060306     c                   if        not *in13 and vidflg = *blanks and
055900060306     c                             vidftc = 'd '
056000060306     c                   movel     'C '          v09ftc
056100061011     c  n12ktpt9         chain(n)  titpt01l
056200061011     c   12ktpt9         chain(n)  tiopt01l
056300061011     c                   if        %found and tptatb = *blanks and
056400060306     c                             tpttma = *blanks and tptapl = *blanks
056500060306     c                   eval      vidmsg = msg(9)
056600060306     c                   eval      *in13 = *on
056700060306     c                   endif
056800060306     c                   endif
056900130104
057000130104      * se scelta della tariffa particolare "f " Fuel Surcharge errore
057100130104      * in caso di manutenzione e tariffa, la cartello è libera
057200130128     c*******            IF        not *in13 and vidflg <> *blanks and
057300130128     c*******                      not *in12 and not *in44 and vidftc = 'f '
057400130128     c*******            eval      vidmsg = 'Impossibile manutenzionare la -
057500130128     c*******                      Tariffa Particolare Fuel Surcharge'
057600130128     c*******            eval      *in13 = *on
057700130128     c*******            ENDIF
057800970401     C**
057900050531    3c                   if        not *in13
058000920529     C                   MOVEL     VIDFTC        VI2FTC
058100920612     C                   MOVEL     VIDDES        VI2DTC
058200980317     C* PULIZIA DELLE SCHIERE CODICI TARIFFE PER LA SINGOLA TARIFFA PARTICOLARE
058300980317     C                   MOVEL     *BLANKS       TTR
058400980317     C                   MOVEL     *BLANKS       DTR
058500980317     C                   MOVEL     *BLANKS       TTP
058600980317     C                   MOVEL     *BLANKS       TSP
058700980317     C                   MOVEL     *BLANKS       WTT
058800980429     C                   MOVEL     *BLANKS       ARR
058900980429     C                   MOVEL     *BLANKS       RPV
059000920831     C*
059100941020     C* CARICO I TIPI TARIFFA VALIDI PER LA CONS.PARTICOL. SELEZIONATA
059200941020     C                   MOVEL     '1P'          COD
059300941020     C                   MOVEL     *BLANKS       KEY
059400941020     C                   MOVEL     VIDFTC        KEY
059500941020     C     KTAB          CHAIN     TABEL                              34
059600050531    4C     *IN34         IFEQ      *OFF
059700941109     C* UTT --> TIPI TARIFFA VALIDI PER LA CONSEGNA SELEZIONATA        A
059800980428     C                   MOVEL     TBLUNI        DS1P
059900130125
060000130125       //?Se non posso manutenzionare le tariffe ed è una tariffa
060100130125       //?particolare NON sbloccata errore
060200130125     c                   IF        wBlocca and §1Pmodtp = *blanks
060300130125     c                   eval      vidmsg = 'Impossibile gestire la Tariffa -
060400130125     c                             Particolare selezionata'
060500130125     c                   eval      *in13 = *on
060600130125     c                   goto      nomantp
060700130125     c                   ENDIF
060800130125
060900980428     C                   MOVEL     §1PTT0        WTT(1)
061000980428     C                   MOVEL     §1PTT1        WTT(2)
061100980428     C                   MOVEL     §1PTT2        WTT(3)
061200980428     C                   MOVEL     §1PTT3        WTT(4)
061300980428     C                   MOVEL     §1PTT4        WTT(5)
061400980428     C                   MOVEL     §1PTT5        WTT(6)
061500980428     C                   MOVEL     §1PTT6        WTT(7)
061600980428     C                   MOVEL     §1PTT7        WTT(8)
061700980428     C                   MOVEL     §1PTT8        WTT(9)
061800980428     C                   MOVEL     §1PTT9        WTT(10)
061900980428     C                   MOVEL     §1PTTV        WTT(11)
062000980317     C*
062100980317     C* CARICO TABELLA CODICI TARIFFA
062200980317     C                   EXSR      CARTAB
062300941020     C*
062400941109     C* WCTS --> SE PIENO, IL DETTAGLIO DELLA TARIFFA PARTICOLARE DEVE
062500941109     C*          AVERE COME CODICE TASSAZ. IL CODICE CONTENUTO IN WCTS
062600050531    5C     PARFIE        IFEQ      'I'
062700941123     C* SE TARIFFA ITALIA DEVO PRENDERE COD.TASS. ITALIA
062800941109     C                   MOVEL     §1PCTS        WCTS
062900941123     C                   CLEAR                   WCTE
063000941109     C                   ELSE
063100050531    6C     PARFIE        IFEQ      'E'
063200941123     C* SE TARIFFA ESTERO DEVO PRENDERE COD.TASS. ESTERO
063300941109     C                   MOVEL     §1PCTE        WCTS
063400941123     C                   CLEAR                   WCTE
063500941109     C                   ELSE
063600941123     C* SI TRATTA DI TARIFFA DI CARTELLO QUINDI POSSONO ESISTERE
063700941123     C*   ENTRAMBI I CODICI TASSAZIONE ESPRESSI IN TABELLA
063800941123     C                   MOVEL     §1PCTS        WCTS
063900941123     C                   MOVEL     §1PCTE        WCTE
064000050531    6C                   ENDIF
064100050531    5C                   ENDIF
064200941109     C*
064300941109     C* SALVO LA MASSIMA % PER TARIFFA A VALORE
064400941109     C                   MOVEL     §1PPTV        WPTV
064500941111     C* 11 ON  - VISUALIZZO IL VALORE MERCE E IL FLAG VALORE MERCE
064600941111     C     §1PVVM        COMP      'S'                                    11
064700941111     C* 28 ON  - VISUALIZZO IL TIPO APPLICAZIONE
064800941111     C     §1PVCA        COMP      'S'                                    28
064900941116     C* 38 ON  - VISUALIZZO IL MINIMO  TARIFFA
065000941116     C     §1PVMN        COMP      'S'                                    38
065100941116     C* 39 ON  - VISUALIZZO IL MASSIMO TARIFFA
065200941116     C     §1PVMX        COMP      'S'                                    39
065300980514     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO FITTIZIO
065400050531     C     VIDFTC        COMP      'C'                                    43
065500060802     C* 82 ON  - VISUALIZZO DATA riferimento prezzo base carburante
065600060802     C     §1pdrf        comp      'S'                                    82
065700060802      * Salvo il flag che mi permette di inserire una tariffa particolare
065800021209      * a qtà anche se la tariffa non è a qtà
065900021209     c                   Movel     §1pFqt        Wfqt
066000050531    4C                   ENDIF
066100941020     C*
066200941110     C                   EXSR      VIDEO2
066300050531    3c                   endif
066400130125
066500130125     c     nomantp       tag
066600920827     C*
066700940913     C     NRR           CHAIN     TA67S02                            31
066800920529     C                   MOVEL     ' '           VIDSCE
066900920708     C*
067000920619     C* 07 ON - PREMUTO  CMD6-AGGIORNAMENTO
067100920619     C   07              MOVEL     'S'           VIDFLG
067200990907     C   07              Z-ADD     UDATE8        VIDDUV
067300920708     C*
067400920618     C* 09 ON - PREMUTO  CMD16-ANNULLAMENTO
067500920708     C   09              MOVEL     ' '           VIDFLG
067600990907     C   09              CLEAR                   VIDDUV
067700050531     c* Se errore, mi posizione sulla tariffa particolare
067800050531     c                   if        *in13
067900050531     c                   seton                                        40
068000050531     c                   z-add     nrr           rec
068100050531     c                   endif
068200050531     c
068300940913     C                   UPDATE    TA67S02
068400050531    2C                   END
068500920708     C*
068600050531     c                   if        not *in13
068700940913     C                   READC     TA67S02                                31
068800050531     c                   else
068900050531     c                   seton                                        31
069000050531     c                   endif
069100050531     c
069200050531    1c                   ENDdo
069300921119     C                   GOTO      INIZIO
069400920529     C*
069500920529     C     FINE          TAG
069600921126     C                   MOVEL     PARAM5        KPJBU
069700120320
069800120320     c                   IF        *in97
069900120320     c                   eval      parm48 = tnsb48ds
070000120320     c                   ENDIF
070100120320
070200990917      * CHIUSURA PGM RICERCA BOLLE
070300990917     C                   CLEAR                   DSBS02
070400990917     C                   MOVEL     'C'           T02TLA
070500990917     C                   CALL      'TIBS02R'
070600990917     C                   PARM                    KPJBA
070700990917     C                   PARM                    DSBS02
070800921126     C*
070900920529     C                   SETON                                        LR
071000920609     C*
071100941110     C*--- CARICAMENTO TABELLE ---------------------------------------*
071200920609     C     CARTAB        BEGSR
071300941110     C****  CODICI TARIFFA E DESCRIZIONI  ****
071400920609     C                   MOVEL     'TR'          COD
071500920609     C                   Z-ADD     1             X                 3 0
071600980317     C* CARICO I TIPI TARIFFE CHE VENGONO GESTITE DALLA SINGOLA TARIFFA
071700980317     C* PARTICOLARE
071800990614    1C     WTT(X)        DOWNE     '  '
071900980317     C                   MOVEL     WTT(X)        KEY
072000980317     C     KTAB          CHAIN     TABEL                              30
072100980317    2C     *IN30         IFEQ      '0'
072200980317     C     TBLFLG        ANDEQ     *BLANKS
072300940921     C                   MOVEL     TBLUNI        DSTR
072400940913     C*
072500980317     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE PARTICOLARI
072600980317    3C     §TRUPA        IFEQ      'S'
072700920612     C                   MOVEL     TBLKEY        TTR(X)
072800941110     C                   MOVEL     §TRDES        DTR(X)
072900950112     C* IMPORTO TARIFFA  IN % O £ ("S"=TARIFFA IN % - " "=TARIFFA IN £)
073000941110     C                   MOVEL     §TRTAP        TTP(X)
073100950119     C* SCAGLIONE CON DECIMALI O SENZA ("S"=CON DECIM." "=SENZA DECIM.)
073200950112     C                   MOVEL     §TRSAP        TSP(X)
073300980429     C* ARROTONDAMENTI
073400980429     C                   MOVEL     §TRARR        ARR(X)
073500980429     C* RAPPORTO PESO VOLUME
073600980429     C                   MOVEL     §TRRPV        RPV(X)
073700980317    3C                   END
073800980316     C*
073900980317    2C                   END
074000980317     C*
074100980317     C                   ADD       1             X
074200980317    1C                   ENDDO
074300980317     C*
074400980317     C                   ENDSR
074500980317     C*-------------------------------------------------------------------------
074600980317     C     CARNAZ        BEGSR
074700950112     C****  CODICI NAZIONE  ****
074800950112     C                   Z-ADD     1             X
074900950112     C                   MOVEL     '15'          COD
075000950112     C     KTAB2         SETLL     TABEL
075100950112     C     KTAB2         READE     TABEL                                  32
075200950112     C*
075300950112     C     *IN32         DOWEQ     *OFF
075400950112     C     TBLFLG        IFEQ      ' '
075500950112     C                   MOVEL     TBLUNI        DS15
075600950112     C     §15ITA        IFEQ      'I'
075700950112     C                   MOVEL     TBLKEY        NAZ(X)
075800950112     C                   ADD       1             X
075900950112     C                   ENDIF
076000950112     C                   ENDIF
076100950112     C*
076200950112     C     KTAB2         READE     TABEL                                  32
076300950112     C                   ENDDO
076400050531     c*
076500950112     C*
076600920609     C                   ENDSR
076700080609
076800080609      *------------------------------------------------------------------------*
076900080609      * Carico filiali abilitate al lasciato avviso
077000080609      *------------------------------------------------------------------------*
077100080609     c     carlav        begsr
077200080609
077300080609     c                   clear                   sklav
077400080609     c                   clear                   xx
077500080609
077600080609     c/exec sql
077700080609     c+ declare lav cursor for select tntbe01l.*
077800080609     c+ from tntbe01l
077900080609     c+ where tbecod='LAV' and tbeatb = ''
078000080609     c/end-exec
078100080609
078200080609     c/exec sql
078300080609     c+ open lav
078400080609     c/end-exec
078500080609
078600080609     c                   do        *hival
078700080609
078800080609     c/exec sql
078900080609     c+ fetch next from lav into :tntbeds
079000080609     c/end-exec
079100080609
079200080609     c                   select
079300080609     c                   when      sqlcod = 100
079400080609     c                   leave
079500080609     c                   when      sqlcod < 0
079600080609     c                   other
079700080609     c                   eval      dlav = tbeuni
079800080609      * carico la schiera della filiali abilitate
079900080616     c                   if        d§lavtar > dateu8
080000080609     c                   iter
080100080609     c                   endif
080200080609     c                   eval      xx = xx + 1
080300080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
080400080609
080500080609     c                   endsl
080600080609
080700080609     c                   enddo
080800080609
080900080609     c/exec sql
081000080609     c+ close lav
081100080609     c/end-exec
081200080609
081300080609
081400080609     c                   endsr
081500920609     C*
081600920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
081700920529     C     CARICA        BEGSR
081800920529     C                   SETOFF                                       3031
081900920529     C* PULIZIA SUBFILE
082000920529     C                   EXSR      PULSF
082100920529     C*
082200920529     C                   Z-ADD     0             NRR               4 0
082300920529     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
082400920529     C                   MOVEL     '1P'          COD
082500920608     C     KTAB2         SETLL     TABEL
082600920608     C     KTAB2         READE     TABEL                                  30
082700970325    1C     *IN30         DOWEQ     '0'
082800990621     C* CONTROLLO VALIDITA' TARIFFA PARTICOLARE PER LA GESTIONE TARIFFE
082900970325    2C     TBLFLG        IFEQ      *BLANKS
083000920827     C                   MOVEL     TBLUNI        DS1P
083100020204      * FLAG DI COMODO PER TABELLA 'SP'
083200020204     C                   CLEAR                   FLAGSP
083300080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
083400080609      * per le filiali non abilitate, accetto cartello
083500051012     c                   If        tblkey = 'c       '  and
083600080609     c                             kscfil <> 888
083700080609     c                   if        %lookup(kscfil:sklav) = *zeros
083800080609     c                             and %lookup(999:sklav) = *zeros
083900051012     c                   goto      legtab
084000080609     c                   endif
084100051012     c                   endif
084200060303      * ESCLUDO la tariffa particolare "d" se tariffa di cartello
084300060303     c                   if        tblkey = 'd       '  and *in44
084400060303     c                   goto      legtab
084500060303     c                   endif
084600051012      *
084700020204      * Carico solo le tariffe particolari che hanno flag <> 'N'
084800020204      * Per tariffa con tipo servizio poste controllo flag §1pfpt
084900020201     C     PARTSP        IFEQ      'P'
085000020201     C     §1PFPT        ANDEQ     'N'
085100020201     C                   GOTO      LEGTAB
085200020201     C                   ENDIF
085300020201      * Per tariffa con Network DPD controllo flag §1pfdp
085400020201     C     FLGNET        IFEQ      'D'
085500020201     C     §1PFDP        ANDEQ     'N'
085600020201     C                   GOTO      LEGTAB
085700020201     C                   ENDIF
085800020201      * Per tariffa con Network FedEx controllo flag §1pffe
085900020201     C     FLGNET        IFEQ      'F'
086000020201     C     §1PFFE        ANDEQ     'N'
086100040428      * tariffa FEDEX abilitata solo per le tariffe di cartello e 44 spento
086200040428      * (44 acceso solo se sto modificando una tariffa di cartello)
086300040428     c     flgnet        oreq      'F'
086400040428     c     §1pffe        andeq     'C'
086500040428     c     *in44         andeq     *off
086600020201     C                   GOTO      LEGTAB
086700020201     C                   ENDIF
086800020201      * Per tariffa Italia controllo flag §1ptco
086900020201     C     PARFIE        IFEQ      'I'
087000020205     C     PARTSP        ANDNE     'P'
087100020201     C     §1PTCO        ANDEQ     'N'
087200020201     C                   GOTO      LEGTAB
087300020201     C                   ENDIF
087400020201      * Per tariffa Estero controllo flag §1pfie
087500020201     C     PARFIE        IFEQ      'E'
087600020205     C     FLGNET        ANDEQ     ' '
087700020201     C     §1PFIE        ANDEQ     'N'
087800020201     C                   GOTO      LEGTAB
087900020201     C                   ENDIF
088000020225      * Per tariffe di cartello verifico che non sia tipo servizio "P"
088100020225     C     PARFIE        IFEQ      ' '
088200020225     C     PARTSP        ANDNE     'P'
088300020225     C     §1PTCO        ANDEQ     'N'
088400020225     C                   GOTO      LEGTAB
088500020225     C                   ENDIF
088600020201      *
088700020201      * Utilizzabile in tariffa
088800000420     C**
088900970325    3C     §1PUTA        IFEQ      'S'
089000970325     C*
089100920529     C                   MOVEL     TBLKEY        VIDFTC
089200920529     C                   MOVEL     §1PDES        VIDDES
089300050531     C                   MOVEL     §1Peli        VIheli
089400020201      *
089500020201      * Per tariffa FedEx controllo se devo recupare la descrizione
089600020201      * dalla tabella SP
089700020201     C     FLGNET        IFEQ      'F'
089800020201     C     §1PDFE        ANDEQ     'S'
089900020204     C                   MOVEL     '1'           FLAGSP            1
090000020201     C                   CLEAR                   DSSP
090100020201     C                   MOVEL     'SP'          COD
090200020201     C                   MOVEL     *BLANKS       KEY
090300020201     C                   MOVEL     VIDFTC        KEY
090400020204     C     KTAB          CHAIN     TABEL                              34
090500020201     C     *IN34         IFEQ      *OFF
090600020201     C                   MOVEL     TBLUNI        DSSP
090700020201     C                   ENDIF
090800020201     C     §SPDFE        IFNE      *BLANKS
090900020201     C                   MOVEL     §SPDFE        VIDDES
091000020201     C                   ENDIF
091100020201     C                   ENDIF
091200020201      *
091300920529     C* CAMPO SELEZIONE
091400920529     C                   MOVEL     ' '           VIDSCE
091500920529     C*
091600061011     C  n12KTPT          CHAIN     TITPT000                           31
091700061011     c   12ktpt          chain     tiopt000                           31
091800990903     C                   Z-ADD     0             VIDDUV
091900970325    5C     *IN31         IFEQ      '0'
092000920529     C* CONTROLLO VALIDITA' TARIFFA
092100970325    6C     TPTATB        IFEQ      ' '
092200920529     C* TARIFFA PARTICOLARE GIA' ESISTENTE
092300920529     C                   MOVEL     'S'           VIDFLG
092400990903     C* VALORIZZO L'ULTIMA DATA VARIAZIONE
092500990903     C*
092600990903     C* GIRO DATA
092700990903     C*
092800060802     C                   Z-ADD     TPTDUV        G08INV
092900060802     C                   MOVEL     '3'           G08ERR
093000990903     C                   CALL      'XSRDA8'
093100990903     C                   PARM                    WLBDAT
093200990903     C*
093300060802     C                   Z-ADD     G08DAT        VIDDUV
093400990903     C*
093500920529     C                   ELSE
093600920529     C* TARIFFA ANNULLATA
093700920708     C                   MOVEL     ' '           VIDFLG
093800970325    6C                   END
093900920529     C*
094000970325   X5C                   ELSE
094100920529     C* TARIFFA PARTICOLARE NON ESISTENTE
094200920708     C                   MOVEL     ' '           VIDFLG
094300970325    5C                   END
094400920529     C*
094500990217     C* SE C'E' LA TARIFFA -->VISUALIZZO
094600130607    5C**** VIDFLG        IFEQ      'S'
094700990217     C* SE NON E' UNA TARIFFA CHE RICHIEDE L'ABILITAZIONE --> VISUAL.
094800990217     C*
094900920529     C                   ADD       1             NRR
095000940913     C                   WRITE     TA67S02
095100130607    5C*****              END
095200010920    3C                   END
095300010920    2C                   END
095400000420     C*
095500000420     C     LEGTAB        TAG
095600020204     C     FLAGSP        IFNE      *BLANKS
095700020201     C                   MOVEL     '1P'          COD
095800020204     C     KTAB          SETGT     TABEL
095900020204     C                   ENDIF
096000920611     C     KTAB2         READE     TABEL                                  30
096100970325    1C                   END
096200920529     C                   Z-ADD     1             REC
096300990621     C*
096400920529     C                   ENDSR
096500920529     C*
096600920529     C*--- PULIZIA SUBFILE -------------------------------------------*
096700920529     C     PULSF         BEGSR
096800920529     C                   SETON                                        35
096900940913     C                   WRITE     TA67C02
097000920529     C                   SETOFF                                       35
097100920529     C                   ENDSR
097200000829     C*---------------------------------------------------------------*
097300000829     C*    CARICO DATI IN SUBFILE COPIA TARIFFE DA CARTELLO           *
097400000829     C*---------------------------------------------------------------*
097500000829     C     COPCAR        BEGSR
097600000829     C                   SETOFF                                       8081
097700000829     C* PULIZIA SUBFILE
097800091104     c                   z-add     1             rec
097900000829     C                   SETON                                        80
098000000829     C                   WRITE     TA67C09
098100000829     C                   SETOFF                                       80
098200000829     C*
098300000829     C                   Z-ADD     0             NR9               4 0
098400000829     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
098500000829     C                   MOVEL     '1P'          COD
098600000829     C     KTAB2         SETLL     TABEL
098700000829     C     KTAB2         READE     TABEL                                  30
098800000829    1C     *IN30         DOWEQ     '0'
098900000829     C* CONTROLLO VALIDITA' TARIFFA PARTICOLARE PER LA GESTIONE TARIFFE
099000000829    2C     TBLFLG        IFEQ      *BLANKS
099100000829     C                   MOVEL     TBLUNI        DS1P
099200020204      * FLAG DI COMODO PER TABELLA 'SP'
099300020204     C                   CLEAR                   FLAGSP
099400080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
099500080609      * per le filiali non abilitate, accetto cartello
099600051012     c                   If        tblkey = 'c       '  and
099700080609     c                             kscfil <> 888
099800080609     c                   if        %lookup(kscfil:sklav) = *zeros
099900080609     c                             and %lookup(999:sklav) = *zeros
100000051012     c                   goto      doptab
100100080609     c                   endif
100200051012     c                   endif
100300130125       //?Se non posso manutenzionare le tariffe non carico le tariffe
100400130125       //?particolari NON sbloccate
100500130125     c                   IF        wBlocca and §1Pmodtp = *blanks
100600130125     c                   goto      doptab
100700130125     c                   ENDIF
100800051012      *
100900020204      * Carico solo le tariffe particolari che hanno flag = 'O'
101000020201      * Per tariffa con tipo servizio poste controllo flag §1pfpt
101100020201     C     PARTSP        IFEQ      'P'
101200020201     C     §1PFPT        ANDNE     'O'
101300020201     C                   GOTO      DOPTAB
101400020201     C                   ENDIF
101500020201      * Per tariffa con Network DPD controllo flag §1pfdp
101600020201     C     FLGNET        IFEQ      'D'
101700020201     C     §1PFDP        ANDNE     'O'
101800020201     C                   GOTO      DOPTAB
101900020201     C                   ENDIF
102000020201      * Per tariffa con Network FedEx controllo flag §1pffe
102100020201     C     FLGNET        IFEQ      'F'
102200020201     C     §1PFFE        ANDNE     'O'
102300020201     C                   GOTO      DOPTAB
102400020201     C                   ENDIF
102500020201      * Per tariffa Italia controllo flag §1ptco
102600020201     C     PARFIE        IFEQ      'I'
102700020205     C     PARTSP        ANDNE     'P'
102800020201     C     §1PTCO        ANDNE     'O'
102900020201     C                   GOTO      DOPTAB
103000020201     C                   ENDIF
103100020201      * Per tariffa Estero controllo flag §1pfie
103200020201     C     PARFIE        IFEQ      'E'
103300020205     C     FLGNET        ANDEQ     ' '
103400020201     C     §1PFIE        ANDNE     'O'
103500020201     C                   GOTO      DOPTAB
103600020201     C                   ENDIF
103700020201      *
103800020201      * Utilizzabile in tariffa
103900000829     C**
104000000829    3C     §1PUTA        IFEQ      'S'
104100000829     C*
104200000829     C*
104300000918     C                   MOVEL     TBLKEY        V09FTC
104400061011     C  n12KTPT9         CHAIN     TITPT000                           31
104500061011     c   12ktpt9         chain     tiopt000                           31
104600000829    4C     *IN31         IFEQ      '0'
104700000829     C* CONTROLLO VALIDITA' TARIFFA
104800000829     C     TPTATB        ANDEQ     ' '
104900000829     C* TARIFFA PARTICOLARE GIA' ESISTENTE
105000000829     C                   GOTO      DOPTAB
105100000829     C*
105200000829    4C                   ENDIF
105300000829     C* TARIFFA ANNULLATA   OPPURE
105400000829     C* TARIFFA PARTICOLARE NON ESISTENTE
105500000829     C*                     SCRIVO IL RECORD NEL SUBFILE
105600000829     C                   MOVEL     §1PDES        V09DES
105700020201      *
105800020201      * Per tariffa FedEx controllo se devo recupare la descrizione
105900020201      * dalla tabella SP
106000020201     C     FLGNET        IFEQ      'F'
106100020201     C     §1PDFE        ANDEQ     'S'
106200020204     C                   MOVEL     '1'           FLAGSP            1
106300020201     C                   CLEAR                   DSSP
106400020201     C                   MOVEL     'SP'          COD
106500020201     C                   MOVEL     *BLANKS       KEY
106600020201     C                   MOVEL     V09FTC        KEY
106700020204     C     KTAB          CHAIN     TABEL                              34
106800020201     C     *IN34         IFEQ      *OFF
106900020201     C                   MOVEL     TBLUNI        DSSP
107000020201     C                   ENDIF
107100020201     C     §SPDFE        IFNE      *BLANKS
107200020201     C                   MOVEL     §SPDFE        V09DES
107300020201     C                   ENDIF
107400020201     C                   ENDIF
107500020201      *
107600000829     C* CAMPO SELEZIONE
107700000829     C                   MOVEL     ' '           V09SCE
107800000829     C                   ADD       1             NR9
107900000829     C                   WRITE     TA67S09
108000000829    3C                   END
108100000829    2C                   END
108200000829     C*
108300000829     C     DOPTAB        TAG
108400020204     C     FLAGSP        IFNE      *BLANKS
108500020201     C                   MOVEL     '1P'          COD
108600020204     C     KTAB          SETGT     TABEL
108700020204     C                   ENDIF
108800000829     C     KTAB2         READE     TABEL                                  30
108900000829    1C                   END
109000000829     C*
109100000829     C* GESTIONE DEL SUBFILE DELLA COPIA TARIFFE
109200000915     C                   SETON                                        81
109300000918     C     NR9           COMP      0                                      80
109400000829     C                   Z-ADD     1             NR9
109500000829     C                   WRITE     TA67T01
109600000829     C                   WRITE     TA67Z09
109700000829     C                   EXFMT     TA67C09
109800000829     C*
109900000829    1C     *INKL         IFNE      *ON
110000070521     c     *in80         andeq     *off
110100000829     C* LEGGO IL SUBFILE
110200000901    2C                   DO        *HIVAL
110300000829     C                   READC     TA67S09                                30
110400000915    3C     *IN30         IFEQ      *ON
110500000829     C                   LEAVE
110600000901    3C                   ENDIF
110700000901    3C     V09SCE        IFEQ      '1'
110800061011     C* CARICO LA DS PER PGM DI COPIA TARIFFA
110900061011     c                   clear                   tnta25ds
111000061011     c  n12              eval      ta25tipo = 'T'
111100061011     c   12              eval      ta25tipo = 'O'
111200061011     c                   eval      ta25ksco = vidcli
111300061011     c                   move      vidctr        ta25ctro
111400061011     c                   move      vidprg        ta25prgo
111500061011     c  n12              eval      ta25tipn = 'T'
111600061011     c   12              eval      ta25tipn = 'O'
111700061011     c                   eval      ta25kscn = vidcli
111800061011     c                   move      vidctr        ta25ctrn
111900061011     c                   move      vidprg        ta25prgn
112000061011     c                   eval      ta25tam = 'N'
112100061011     c                   eval      ta25fie = parfie
112200061011     c                   eval      ta25tam = 'N'
112300061011     c                   eval      ta25tad = 'N'
112400061011     c                   eval      ta25tpt = 'N'
112500061011     c                   eval      ta25tpd = 'N'
112600061011     c                   eval      ta25tgc = 'N'
112700061011     c                   eval      ta25cat = 'N'
112800061011     c                   eval      ta25inglo = 'S'
112900061011
113000061011     c                   eval      ta25ftcr = v09ftc
113100061011
113200061011     c                   call      'TNTA25R'
113300061011     c                   parm                    kpjba
113400061011     c                   parm                    tnta25ds
113500061011     c                   parm                    tnta25tad
113600061011     c                   parm                    tnta25tgc
113700061011     c                   parm                    tnta25tpd
113800061011     c                   parm                    tnta25tpt
113900000829     C*
114000000901    3C                   ENDIF
114100000901    2C                   ENDDO
114200000829     C*
114300000901    1C                   ENDIF
114400000829     C*
114500000829     C                   ENDSR
114600920529     C*
114700920529     C*--- SR SECONDA VIDEATA ----------------------------------------*
114800920529     C     VIDEO2        BEGSR
114900920619     C                   SETOFF                                       010207
115000920619     C                   SETOFF                                       090304
115100941116     C                   SETOFF                                       062425
115200060303
115300060303     c                   eval      *in78 = *off
115400060303
115500920529     C*
115600941116     C* 24 ON - TIPO CONSEGNA = "C - ASSICURAZIONE X CONTO"
115700990217     C     §1PASC        IFEQ      'S'
115800990217     C                   SETON                                            24
115900990217     C                   ENDIF
116000941116     C* 25 ON - TIPO CONSEGNA = "R - RCV"
116100990217     C     §1PRCV        IFEQ      'S'
116200990217     C                   SETON                                            25
116300990217     C                   ENDIF
116400941110     C*
116500920609     C* TARIFFA PARTICOLARE NON ESISTENTE
116600920708     C     VIDFLG        IFEQ      ' '
116700920529     C                   SETON                                        01
116800920529     C                   ELSE
116900920609     C* TARIFFA PARTICOLARE GIA' ESISTENTE
117000920529     C                   SETON                                        02
117100920529     C                   END
117200920529     C*
117300921006     C* PULIZIA CAMPI DEL FORMATO E DEL SFLCONTROL
117400921006     C                   MOVEL     ' '           VI2TPG
117500920529     C                   EXSR      PULCTL
117600921126     C*
117700921005     C* CARICO TIPO TARIFFA
117800061011     C   02
117900061011     Cann12KTPT          CHAIN     TITPT000                           33
118000061011     c   02
118100061011     can 12ktpt          chain     tiopt000                           33
118200921005     C   02
118300921005     CANN33              MOVEL     TPTTPG        VI2TPG
118400921005     C   02
118500921005     CANN33              MOVEL     TPTTPG        SAVTPG
118600921118     C   01              MOVEL     ' '           SAVTPG
118700060303
118800060303      * se tariffa particolare "d"
118900060303      * i dati vanno protetti
119000060303     c                   if        vidftc = 'd '
119100060303     c                   eval      *in78 = *on
119200060303      * se non è presente carico i dati di dft
119300060303     c                   if        *in01
119400060303     c                   eval      vi2tpg = '3'
119500060303     c                   endif
119600060303     c                   endif
119700921005     C*
119800921005     C     FOR05         TAG
119900921005     C                   SETOFF                                       22
120000960520     C*
120100940913     C                   WRITE     TA67T01
120200940913     C                   WRITE     TA67Z08
120300921005     C     FOR06         TAG
120400941110     C                   EXFMT     TA67D06
120500921005     C*
120600921005     C** CMD12 - FINE LAVORO
120700921005     C   KL              GOTO      FINE2
120800921005     C*
120900921005     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
121000921005     C   KQ              EXSR      ANNTAR
121100921005     C   KQ              GOTO      FINE2
121200921005     C*
121300921005     C* CONTROLLI FORMATO
121400921005     C                   EXSR      CONTR2
121500921005     C   98              GOTO      FOR05
121600921005     C*
121700921005     C   99              GOTO      FOR06
121800921005     C*
121900921005     C     INVID2        TAG
122000921007     C                   Z-ADD     0             NR2
122100921007     C                   Z-ADD     0             SAVNR2
122200921005     C                   SETON                                        22
122300921005     C* CARICO DATI DEL SFLCONTROL
122400921215     C   02
122500921215     CANN29              MOVEL     TPTDS         COMTPT
122600921006     C   02
122700921006     CANN29              EXSR      CARCTL
122800060303
122900060303      * se tariffa particolare "d" non presente carico i dati di dft
123000060303     c                   if        *in01 and vidftc = 'd '
123100060303      * valore merce 6,2
123200060303     c                   z-add     6,2           vi2vlm
123300060303     c                   z-add     vi2vlm        savvlm
123400060303      * tipo valore '3' già impostato in precedenza se immissione
123500060303     c                   endif
123600060802      * se tariffa particolare "f" non presente carico i dati di dft
123700060802     c                   if        *in01 and vidftc = 'f '
123800060802     c                   seton                                        86
123900061207      *? ATTENZIONE CHIODO!!!  ?
124000061207      * se data del giorno è inferiore a 31/12/2006
124100061207      * chiodo fisso 31/12/2006
124200061207      * altrimenti metto la data del giorno
124300140219     c****               if        dateu8 < 20061231
124400140219     c****               eval      vi2dpb = 31122006
124500140219     c****               else
124600140219     c****               z-add     udate8        vi2dpb
124700140219     c****               endif
124800131216      * verifico se esiste il file del prezzo base da impostare in base alla
124900131216      * data validità
125000131217     c     dateu8        setll     tidpb01l
125100131216     c                   read      tidpb01l
125200131216     c                   if        %found(tidpb01l)
125300140220     c                   z-add     dpbdpb        vo2dpb
125400140115     c                   eval      VI2pma = DPBpma
125500131216     c                   endif
125600131216      * recupero il prezzo del gasolio corrispondente
125700131218     c                   eval      Kpmg = dpbdpb
125800131218     c                   exsr      Rec_pmg
125900131216
126000131218     c                   endif
126100060303      *
126200920611     C* PULIZIA SUBFILE
126300920611     C                   EXSR      PULSF2
126400941110     C*
126500970520     C* 06 ON - APPLICAZIONE CON INOLTRO  PER ISOLE/LOC.DISAGIATE/C.STO
126600970520     C     §1PAIN        COMP      'S'                                    06
126700941110     C*
126800920529     C* CARICO SUBFILE
126900921214     C   02              EXSR      CARSFL
127000920609     C* SCRIVO RECORD DI NUOVA IMMISSIONE
127100921005     C   01              EXSR      SCRIVI
127200920617     C*
127300920617     C                   Z-ADD     1             REC2
127400920611     C     FOR01         TAG
127500940913     C                   WRITE     TA67T01
127600940913     C                   WRITE     TA67Z05
127700941110     C                   WRITE     TA67D06
127800920529     C     FORCT         TAG
127900941110     C                   EXFMT     TA67C07
128000950119     C*
128100950119     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN13)
128200950119     C                   CLEAR                   VI2MSG
128300950119     C                   SETOFF                                       1374
128400140131     c                   setoff                                       62
128500920609     C*
128600111122     C** CMD12 - FINE LAVORO
128700111122     c                   If        *Inkl  and *in62
128800111122     C                   SETON                                        99
128900111122     c                   goto      forct
129000111122     C                   ENDIF
129100111122     c
129200920611     C** CMD12 - FINE LAVORO
129300050704     c                   If        *Inkl
129400111122      /free
129500111122       // Non posso fare F12 se ho annullato tutte le righe di dettaglio
129600111130        IF not *in01;
129700111122         *in62 = *on;
129800111123         nr2 = 1;
129900111123         chain nr2 TA67S07;
130000111123         DOW %found and *in62;
130100111130           IF  VI2cts <> *blanks;
130200111123             *in62 = *off;
130300111123           ENDIF;
130400111123           update TA67S07;
130500111122           nr2 += 1;
130600111122           chain nr2 TA67S07;
130700111122         ENDDO;
130800111123      /end-free
130900111123     c                   IF        *in62
131000111123     c                   eval      *in99 = *in62
131100111123     c                   goto      forct
131200111123     c                   ENDIF
131300111130      /free
131400111130          *in62 = *on;
131500111130          If not *in12 ;
131600111130          setll (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
131700111130          reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
131800111130          DOW  not %eof(TITPD01L);
131900111130            IF  TPDatb = *blanks;
132000111130              *IN62 = *off;
132100111130            ENDIF;
132200111130            reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
132300111130          ENDDO;
132400111130          Endif ;
132500111130          If *in12 ;
132600111130            setll (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
132700111130            reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
132800111130            DOW  not %eof(TIOPD01L);
132900111130              IF  TPDatb = *blanks;
133000111130               *IN62 = *off;
133100111130             ENDIF;
133200111130             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
133300111130           ENDDO;
133400111130           Endif ;
133500111130      /end-free
133600111130     c                   IF        *in62
133700111130     c                   eval      *in99 = *in62
133800111130     c                   goto      forct
133900111130     c                   ENDIF
134000111130     c                   ENDIF
134100111122     c
134200050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
134300050704      * per codice di tassazione
134400050704     c                   Clear                   savcts
134500050704     c                   If        vi2tpg = '2'
134600061011     c  n12ktpt          Setll     titpd01l
134700061011     c   12ktpt          Setll     tiopd01l
134800050704     c                   Do        *Hival
134900061011     c  n12ktpt          Reade(n)  titpd01l
135000061011     c   12ktpt          Reade(n)  tiopd01l
135100061011     c                   If        %eof
135200050704     c                   Leave
135300050704     c                   EndIf
135400050704     c                   If        tpdatb <> *Blanks
135500050704     c                   Iter
135600050704     c                   EndIf
135700050704     c                   If        tpdcts = savcts
135800050704     c                   Eval      *in69 = *on
135900050704     c                   Eval      *in13 = *on
136000050704     c                   Eval      vi2msg = msg(6)
136100050704     c                   Else
136200050704     c                   Eval      savcts = tpdcts
136300050704     c                   EndIf
136400050704     c                   EndDo
136500050704     c                   If        *In13
136600050704     c                   Goto      invid2
136700050704     c                   EndIf
136800050704     c                   EndIf
136900050704     C                   GOTO      FOR05
137000050704     c                   EndIf
137100920611     C*
137200920611     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
137300920611     C   KQ              EXSR      ANNTAR
137400920611     C   KQ              GOTO      FINE2
137500140220     C** CMD04 - Interrogazione prezzi del gasolio
137600140220     C   KD              EXSR      RecPrezzi
137700140220     C   KD              GOTO      Forct
137800920611     C*
137900920611     C** CMD9  - AGGIUNTA RIGHE
138000920611     C   KI
138100920529     C* SCORRIMENTO
138200920611     COR 08              DO
138300920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
138400920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
138500920619     C                   SETON                                        0336
138600920611     C                   EXSR      SCRIVI
138700920529     C                   GOTO      FORCT
138800920529     C                   END
138900920529     C*
139000920611     C* CONTROLLO DATI SUBFILE CONTROL
139100920611     C                   EXSR      CONCTL
139200941111     C   98              GOTO      FOR01
139300941111     C*
139400920619     C   99              GOTO      FORCT
139500920529     C*
139600921007     C* CONTROLLO DATI SUBFILE
139700921007     C                   EXSR      CONSFL
139800921007     C   99              GOTO      FORCT
139900141118      * se sto manutenzionando la tariffa FUEL ed ho premuto il tasto F13
140000141118      * calcolo fuel richiamo un pgm esterno passandogli gli scaglioni
140100141118     c                   If        *inkm and *in82
140200141118     c                   exsr      calc_fuel
140300141118     c                   goto      forct
140400141118     c                   endif
140500140131
140600921007     C*
140700920623     C** ENTER - REGISTRA
140800920624     C* REGISTRO CAMPI DEL SUBFILE
140900920709     C                   EXSR      REGIS
141000920624     C* REGISTRO CAMPI DEL SUBFILE CONTROL
141100920709     C                   EXSR      REGIS2
141200920623     C*
141300921007     C* SPENGO L' *IN29  PERCHE' IN QUESTO MODO RIESCO A POSIZIONARE
141400921007     C*   IL CURSORE SULLA PRIMA RIGA DI DETTAGLIO  - (ALTRIMENTI
141500921007     C*   RESTAVA POSIZIONATO SUL PRIMO CAMPO DEL SFLCONTROL) -
141600921007     C                   SETOFF                                       29
141700111123      /free
141800111123       // Se ho dato F6 o F14 verifico di avere almeno 1 rcd di dettaglio
141900111123         IF  *inkf or *inkn;
142000111123           *in62 = *on;
142100111130           If not *in12 ;
142200111123           setll (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
142300111123           reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
142400111123           DOW  not %eof(TITPD01L);
142500111123             IF  TPDatb = *blanks;
142600111123               *IN62 = *off;
142700111123             ENDIF;
142800111123             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TITPD01L;
142900111123           ENDDO;
143000111130           Endif ;
143100111130           If *in12 ;
143200111130           setll (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
143300111130           reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
143400111130           DOW  not %eof(TIOPD01L);
143500111130             IF  TPDatb = *blanks;
143600111130               *IN62 = *off;
143700111130             ENDIF;
143800111130             reade(n) (VIDcli : VIDctr : VIDprg : VIDftc) TIOPD01L;
143900111130           ENDDO;
144000111130           Endif ;
144100111123         ENDIF;
144200140131      /end-free
144300140131     c                   IF        *in62
144400140131     c                   eval      *in99 = *in62
144500140131     c                   goto      invid2
144600140131     c                   ENDIF
144700050704
144800050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
144900050704      * per codice di tassazione
145000050704     c                   Clear                   savcts
145100050704     c                   If        vi2tpg = '2'
145200061011     c  n12ktpt          Setll     titpd01l
145300061011     c   12ktpt          Setll     tiopd01l
145400050704     c                   Do        *Hival
145500061011     c  n12ktpt          Reade(n)  titpd01l
145600061011     c   12ktpt          Reade(n)  tiopd01l
145700061011     c                   If        %eof
145800050704     c                   Leave
145900050704     c                   EndIf
146000050704     c                   If        tpdatb <> *Blanks
146100050704     c                   Iter
146200050704     c                   EndIf
146300050704     c                   If        tpdcts = savcts
146400050704     c                   Eval      *in69 = *on
146500050704     c                   Eval      *in13 = *on
146600050704     c                   Eval      vi2msg = msg(6)
146700050704     c                   Else
146800050704     c                   Eval      savcts = tpdcts
146900050704     c                   EndIf
147000050704     c                   EndDo
147100050704     c                   If        *In13
147200050704     c                   Goto      invid2
147300050704     c                   EndIf
147400050704     c                   EndIf
147500050704
147600921007     C*
147700920623     C** CMD6  - REGISTRA ED ESCE
147800921202     C  NKF
147900921202     CANNKN              GOTO      INVID2
148000920529     C*
148100920708     C     FINE2         ENDSR
148200920609     C*
148300920609     C*--- PULIZIA SUBFILE CONTROL -----------------------------------*
148400920609     C     PULCTL        BEGSR
148500930223     C                   MOVEL     ' '           UNO               1
148600920609     C                   Z-ADD     0             VI2ARL
148700940712     C                   Z-ADD     0             VI2ARF
148800920609     C                   Z-ADD     0             VI2ARO
148900930217     C                   Z-ADD     0             VI2RPV
149000980422     C                   MOVE      ' '           VI2APL
149100980428     C                   MOVE      ' '           VI2MAS
149200140115     c                   clear                   VI2pma
149300140219     C                   CLEAR                   VO2dpb
149400140219     C                   CLEAR                   VO2sca
149500140219     C                   CLEAR                   Vi2pmg
149600941111     C                   CLEAR                   TPG1
149700941111     C                   CLEAR                   DPG1
149800941111     C                   CLEAR                   TPG2
149900941111     C                   CLEAR                   DPG2
150000941111     C                   CLEAR                   TPG3
150100941111     C                   CLEAR                   DPG3
150200941111     C                   CLEAR                   TPG4
150300941111     C                   CLEAR                   DPG4
150400941111     C                   CLEAR                   TPG5
150500941111     C                   CLEAR                   DPG5
150600941111     C                   CLEAR                   TPG6
150700941111     C                   CLEAR                   DPG6
150800980508     C                   CLEAR                   TPG7
150900980508     C                   CLEAR                   DPG7
151000980508     C                   CLEAR                   TPG8
151100980508     C                   CLEAR                   DPG8
151200941111     C*
151300941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
151400941111    1C     *IN11         IFEQ      '1'
151500930217     C   24              Z-ADD     0             VI2VLM
151600930217     C*
151700930217     C* IN IMMISSIONE PER IL TIPO CONSEGNA "R", NEL CAMPO LIMITE
151800941111     C*    RISARCIBILE PROPONGO 12000 PRENDENDOLO DALLA TABELLA "1G"
151900990907     C   25              Z-ADD     §GELRP        VI2VLM
152000930217     C*
152100930223     C* IN IMMISSIONE PROPONGO IL CAMPO TIPO VALORE = 3-PESO KG.
152200930223     C                   MOVEL     '3'           VI2FVM
152300941111     C* DECODIFICA TIPO VALORE MERCE
152400930223     C                   MOVEL     *BLANKS       DESFVM
152500980316     C* DECODIFICA DALLA TABELLA TR
152600980316     C                   MOVEL     'TR'          COD
152700930223     C                   MOVEL     *BLANKS       KEY
152800930223     C                   MOVEL     VI2FVM        KEY
152900930223     C     KTAB          CHAIN     TABEL                              34
153000941111    2C     *IN34         IFEQ      '0'
153100980316     C                   MOVEL     TBLUNI        DSTR
153200980316     C* DECODIFICA DALLA TABELLA TR
153300980316     C                   MOVEL     §TRDES        DESFVM
153400941111    2C                   END
153500941111    1C                   END
153600941110     C*
153700980317     C* CARICO I TIPI TARIFFA
153800941110     C     TTR(1)        IFNE      ' '
153900941110     C                   MOVEL     TTR(1)        TPG1
154000941110     C                   MOVEL     DTR(1)        DPG1
154100941110     C                   END
154200941110     C     TTR(2)        IFNE      ' '
154300941110     C                   MOVEL     TTR(2)        TPG2
154400941110     C                   MOVEL     DTR(2)        DPG2
154500941110     C                   END
154600941110     C     TTR(3)        IFNE      ' '
154700941110     C                   MOVEL     TTR(3)        TPG3
154800941110     C                   MOVEL     DTR(3)        DPG3
154900941110     C                   END
155000941110     C     TTR(4)        IFNE      ' '
155100941110     C                   MOVEL     TTR(4)        TPG4
155200941110     C                   MOVEL     DTR(4)        DPG4
155300941110     C                   END
155400941110     C     TTR(5)        IFNE      ' '
155500941110     C                   MOVEL     TTR(5)        TPG5
155600941110     C                   MOVEL     DTR(5)        DPG5
155700941110     C                   END
155800941110     C     TTR(6)        IFNE      ' '
155900941110     C                   MOVEL     TTR(6)        TPG6
156000941110     C                   MOVEL     DTR(6)        DPG6
156100941110     C                   END
156200980428     C     TTR(7)        IFNE      ' '
156300980428     C                   MOVEL     TTR(7)        TPG7
156400980428     C                   MOVEL     DTR(7)        DPG7
156500980428     C                   END
156600980428     C     TTR(8)        IFNE      ' '
156700980428     C                   MOVEL     TTR(8)        TPG8
156800980428     C                   MOVEL     DTR(8)        DPG8
156900980428     C                   END
157000941110     C*
157100941111     C                   MOVEL     *BLANKS       COMTPT           38
157200920828     C                   ENDSR
157300140220     C*
157400140220     C*--- Recupero Prezzi del Gasolio -------------------------------*
157500140220     C     RecPrezzi     BEGSR
157600140220
157700140220     c                   clear                   tnta39ds
157800140220     c                   call      'TNTA39R'
157900140220     c                   parm                    kpjba
158000140220     c                   parm                    tnta39ds
158100140220
158200140220     c                   If        ota39f03 <> '1'
158300140220     c                   eval      vi2pmg = ota39pmg
158400140220     c                   eval      vo2dpb = ota39dtd
158500140220     c                   eval      vo2sca = ota39sca
158600140220     c                   Endif
158700140220
158800140220     C                   ENDSR
158900921005     C*
159000941116     C*--- CONTROLLO DATI FORMATO ------------------------------------*
159100921005     C     CONTR2        BEGSR
159200921006     C                   SETOFF                                       262729
159300941110     C                   SETOFF                                       989921
159400060802     C                   SETOFF                                       14
159500950119     C                   CLEAR                   WSCD
159600921005     C*
159700921005     C* TIPO TARIFFA OBBLIGATORIO
159800941116    1C     VI2TPG        IFEQ      '?'
159900941110     C                   MOVEL     'TR'          COD
160000941110     C*
160100921005     C                   MOVE      ' '           VI2TPG
160200001020     C                   MOVE      *BLANKS       KEY
160300921005     C                   MOVEL     *BLANKS       DES              25
160400921005     C                   CALL      'X§TABER'
160500921005     C                   PARM                    CODUT
160600921005     C                   PARM                    COD
160700921005     C                   PARM                    KEY
160800921005     C                   PARM                    DES
160900921005     C                   MOVEL     KEY           VI2TPG
161000941110     C                   SETON                                        98
161100921005     C                   GOTO      END2
161200941116    1C                   END
161300921005     C*
161400941116    1C     VI2TPG        IFEQ      ' '
161500921005     C                   SETON                                        4099
161600941116    1C                   END
161700941116     C*
161800921005     C* CONTROLLO SE ESISTE TIPO TARIFFA
161900941020     C                   Z-ADD     1             X
162000941020     C     VI2TPG        LOOKUP    TTR(X)                                 20
162100941110     C* CONTROLLO SE TARIFFA IN % O IN £
162200941110     C   20TTP(X)        COMP      'S'                                    14
162300950119     C* WSCD = "S" ---> SCAGLIONE CON DECIMALI
162400950119     C   20              MOVEL     TSP(X)        WSCD              1
162500980429     C* ARROTONDAMENTI
162600980429     C   20              MOVEL     ARR(X)        WARR              1
162700980429     C* RAPPORTO PESO VOLUME
162800980429     C   20              MOVEL     RPV(X)        WRPV              1
162900941110     C*
163000941110     C*
163100921005     C  N20              SETON                                        4199
163200921005     C   99              GOTO      END2
163300941020     C*
163400950119     C* I TIPI TARIFFA "4" E "5" SI POSSONO ACCETTARE SOLO SE LA TARIFFA
163500950119     C*   CLIENTE HA COME COD.TARIFFA RISPETTIVAMENTE "4xx" E "5xx"
163600021209      * se in tabella '1P' il flag §1PFQT è a Blank
163700021209     c                   If        Wfqt = *Blanks
163800950119     C                   MOVEL     VIDCTR        WCTR              1
163900950119    1C     VI2TPG        IFEQ      '4'
164000950119     C     WCTR          ANDNE     '4'
164100950119     C                   SETON                                        7099
164200950119     C                   GOTO      END2
164300950119    1C                   ENDIF
164400950119    1C     VI2TPG        IFEQ      '5'
164500950119     C     WCTR          ANDNE     '5'
164600950119     C                   SETON                                        7399
164700950119     C                   GOTO      END2
164800950119    1C                   ENDIF
164900021209     c                   EndIf
165000950119     C*
165100950112     C* IN VARIAZIONE: SE TARIFFA A % NON POSSO VARIARE IL TIPO TARIFFA
165200950119    1C   02SAVTPG        IFNE      VI2TPG
165300950119     C                   CLEAR                   WCOM
165400950119     C                   Z-ADD     1             X
165500950119     C*
165600950119     C     SAVTPG        LOOKUP    TTR(X)                                 20
165700950119     C* CONTROLLO SE TARIFFA IN % O IN £
165800950119     C   20TTP(X)        IFEQ      'S'
165900950119     C                   MOVEL     'P'           WCOM              1
166000950119     C                   ENDIF
166100950119     C*
166200950119    2C     *IN14         IFEQ      *ON
166300950119     C     WCOM          ANDEQ     ' '
166400950119     C     *IN14         OREQ      *OFF
166500950119     C     WCOM          ANDEQ     'P'
166600941116     C                   SETON                                        5199
166700941116     C                   GOTO      END2
166800950112    2C                   ENDIF
166900950112    1C                   ENDIF
167000941110     C*
167100921016     C* OGNI VOLTA CHE CAMBIO IL TIPO TARIFFA DEVO AZZERARE TUTTI
167200921016     C*   I CAMPI DEL SFLCONTROL
167300941116    1C     VI2TPG        IFNE      SAVTPG
167400921006     C                   SETON                                        29
167500921005     C                   EXSR      PULCTL
167600941116    1C                   END
167700921005     C*
167800921005     C* CONDIZIONO L'EMISSIONE DEGLI ARROTOND. E DEL RAPPORTO PESO/VOL.
167900980429     C     WARR          IFEQ      'S'
168000980429     C                   SETON                                        26
168100980429     C                   END
168200980429     C*
168300980429     C     WRPV          IFEQ      'S'
168400980429     C                   SETON                                        27
168500980429     C                   END
168600941110     C*
168700941110     C* 21 ON - SE TIPO TARIFFA = 0  PROTEGGO IL CAMPO TIPO VALORE
168800981027     C   25              SETON                                        21
168900921005     C*
169000921005     C     END2          ENDSR
169100920529     C*
169200920611     C*--- PULIZIA SUBFILE -------------------------------------------*
169300920611     C     PULSF2        BEGSR
169400920611     C                   SETON                                        37
169500941111     C                   WRITE     TA67C07
169600920611     C                   SETOFF                                       37
169700920611     C                   ENDSR
169800920611     C*
169900920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
170000921214     C     CARSFL        BEGSR
170100920609     C                   SETOFF                                       323399
170200050704     c                   clear                   savcts
170300920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
170400920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
170500920619     C                   SETON                                        050436
170600920616     C*
170700920611     C                   MOVEL     'CT'          COD
170800920611     C                   MOVEL     *BLANKS       KEY
170900920529     C* RICERCA
171000061011     C  n12KTPT          SETLL     TITPD000
171100061011     c   12ktpt          setll     tiopd000
171200920608     C                   Z-ADD     0             NR2               4 0
171300061011     C  n12KTPT          READE     TITPD000                               32
171400061011     c   12ktpt          reade     tiopd000                               32
171500920529     C* 32 ON - FINE LETTURA
171600920611     C     *IN32         DOWEQ     '0'
171700020207      *
171800050704     C                   SETOFF                                       767769
171900020207      *
172000920611     C* CONTROLLO VALIDITA' TARIFFA
172100920611     C     TPDATB        IFEQ      ' '
172200920529     C* CODICE TASSAZIONE
172300920529     C                   MOVEL     TPDCTS        VI2CTS
172400920608     C* DECODIFICA CODICE TASSAZIONE
172500920609     C                   MOVEL     VI2CTS        KEY
172600920608     C     KTAB          CHAIN     TABEL                              33
172700920612     C  N33              DO
172800920612     C                   MOVEL     TBLUNI        DSCT
172900920612     C                   MOVEL     §CTDES        VI2DCT
173000920612     C                   END
173100920608     C   33              MOVEL     *BLANKS       VI2DCT
173200020207      *
173300020207      * Se è tariffa FedEx devo copiare solo i codici tassazioni
173400020207      * previsti per FedEx
173500020207     C     FLGNET        IFEQ      'F'
173600020207     C     §CTUTF        ANDNE     'S'
173700020207     C                   SETON                                        76
173800020207     C     VI2MSG        IFEQ      *BLANKS
173900020207     C                   SETON                                        13
174000020207     C                   MOVEA     MSG(2)        VI2MSG
174100020207     C                   ENDIF
174200020207     C                   ENDIF
174300020207      * Se NON è tariffa FedEx devo copiare solo i codici tassazioni
174400020207      * NON previsti per FedEx
174500020207     C     FLGNET        IFNE      'F'
174600020207     C     §CTUTF        ANDEQ     'S'
174700020207     C                   SETON                                        77
174800020207     C     VI2MSG        IFEQ      *BLANKS
174900020207     C                   SETON                                        13
175000020207     C                   MOVEA     MSG(3)        VI2MSG
175100020207     C                   ENDIF
175200020207     C                   ENDIF
175300050704      *
175400050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
175500050704      * per codice di tassazione
175600050704     c                   If        vi2tpg = '2'
175700050704     c                   If        tpdcts = savcts
175800050704     c                   Eval      *In69 = *On
175900050704     c                   If        vi2msg = *Blanks
176000050704     c                   Eval      *In13 = *On
176100050704     c                   Movea     msg(6)        vi2msg
176200050704     c                   EndIf
176300050704     c                   Else
176400050704     c                   Eval      savcts = tpdcts
176500050704     c                   EndIf
176600050704     c                   EndIf
176700020207      *
176800970318     C                   MOVEL     TPDORP        VI2ORP
176900920615     C* SCAGLIONE
177000940712     C                   MOVEL     TPDSGL        VI2SGL
177100920608     C* IMPORTO TARIFFA
177200941110     C                   Z-ADD     TPDITR        VI2ITR
177300941110     C                   Z-ADD     TPDITR        SAVITR
177400960520     C                   Z-ADD     TPDITR        SA2ITR
177500920608     C* MINIMO TARIFFA
177600990614     C                   Z-ADD     TPDMIN        VI2MIN
177700990614     C                   Z-ADD     TPDMIN        SAVMIN
177800940713     C* MASSIMO TARIFFA
177900990614     C                   Z-ADD     TPDMAX        VI2MAX
178000990614     C                   Z-ADD     TPDMAX        SAVMAX
178100920611     C* 06 ON - TIPO CONSEGNA = I  CARICO FLAG TARIFFA PROVINCIA
178200920611     C   06              MOVEL     TPDAIN        VI2AIN
178300930219     C   06              MOVEL     TPDAIN        SAVAIN
178400930219     C  N06              MOVEL     ' '           SAVAIN
178500920610     C*
178600061011     C  n12              Z-ADD     TPDNRR        VI2NRR
178700061011     c   12              z-add     tponrr        vi2nrr
178800920608     C* FLAG ANNULLAMENTO
178900920610     C                   MOVEL     ' '           VI2ATB
179000920610     C*
179100920608     C                   ADD       1             NR2
179200920608     C*
179300941111     C                   WRITE     TA67S07
179400920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
179500920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
179600920709     C                   SETOFF                                       0436
179700920611     C                   END
179800920608     C*
179900061011     C  n12KTPT          READE     TITPD000                               32
180000061011     c   12ktpt          reade     tiopd000                               32
180100920611     C                   END
180200920608     C*
180300920609     C                   Z-ADD     NR2           SAVNR2            4 0
180400920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
180500920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
180600920619     C                   SETOFF                                       0436
180700920608     C*
180800920616     C     NR2           IFGT      0
180900920608     C     NR2           DIV       9             DIVIS             3 0
181000920608     C                   MVR                     RESTO             3 0
181100920611     C     RESTO         IFGT      0
181200920611     C                   EXSR      SCRIVI
181300920611     C                   END
181400921126     C*
181500920616     C                   ELSE
181600920618     C                   SETON                                        03
181700920616     C                   EXSR      SCRIVI
181800920616     C                   END
181900920611     C*
182000920608     C                   ENDSR
182100920608     C*
182200920609     C*--- CARICO DATI SUBFILE CONTROL -------------------------------*
182300920608     C     CARCTL        BEGSR
182400061011     C  n12KTPT          CHAIN     TITPT000                           33
182500061011     c   12ktpt          chain     tiopt000                           33
182600941111    1C     *IN33         IFEQ      '0'
182700140115     c                   eval      dTPT01 = TPTflo
182800920925     C*
182900941111     C* 26 ON  - VISUALIZZO ARROTONDAMENTI
183000941111    2C     *IN26         IFEQ      *ON
183100990614     C                   Z-ADD     TPTARL        VI2ARL
183200990614     C                   Z-ADD     TPTARF        VI2ARF
183300990614     C                   Z-ADD     TPTARO        VI2ARO
183400941111    2C                   END
183500941111     C* 27 ON  - VISUALIZZO RAPPORTO PESO/VOLUME
183600920925     C   27              Z-ADD     TPTRPV        VI2RPV
183700920924     C*
183800941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
183900941111    2C     *IN11         IFEQ      *ON
184000920924     C* VALORE MERCE/LIMITE RISARCIBILE
184100920924     C                   Z-ADD     TPTVLM        VI2VLM
184200921009     C                   Z-ADD     TPTVLM        SAVVLM
184300941111     C* TIPO VALORE MERCE
184400940712     C                   MOVEL     TPTFVM        VI2FVM
184500941111     C* DECODIFICA TIPO VALORE MERCE
184600921007     C                   MOVEL     *BLANKS       DESFVM
184700980316     C                   MOVEL     'TR'          COD
184800921007     C                   MOVEL     *BLANKS       KEY
184900921007     C                   MOVEL     VI2FVM        KEY
185000921007     C     KTAB          CHAIN     TABEL                              34
185100941111    3C     *IN34         IFEQ      '0'
185200980316     C                   MOVEL     TBLUNI        DSTR
185300980316     C                   MOVEL     §TRDES        DESFVM
185400941111    3C                   END
185500941111    2C                   END
185600941111     C*
185700941111     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
185800941111     C   28              MOVEL     TPTAPL        VI2APL
185900990217     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO
186000990903     C   43              MOVEL     TPTTMA        VI2MAS
186100140219      * 82 ON  - Visualizzo il prezzo base di riferimento del carburante
186200140219      *          E IL RELATIVO scaglione e la data non visualizzata
186300060802     c                   If        *in82
186400140219     c                   z-add     tptdpb        vo2dpb
186500140115     c                   IF        %check(c_Digits:%subst(TPTflo:1:
186600140115     c                             %len(§TPTpma))) = *zeros
186700140115     c                   eval      VI2pma = §TPTpma
186800140115     c                   ENDIF
186900060802     c                   endif
187000131216      * recupero il prezzo del gasolio corrispondente
187100131218     c                   eval      Kpmg = TPTdpb
187200131218     c                   exsr      Rec_pmg
187300131218
187400941111    1C                   END
187500060802     C
187600920924     C*
187700920608     C                   ENDSR
187800920609     C*
187900920609     C*--- SR AGGIUNTA RIGHE -----------------------------------------*
188000920609     C     SCRIVI        BEGSR
188100920611     C                   SETOFF                                       05
188200020207     C                   SETOFF                                       7677
188300920611     C                   Z-ADD     0             B                 3 0
188400920611     C     RESTO         IFNE      0
188500920611     C                   Z-ADD     RESTO         B
188600920611     C                   Z-ADD     0             RESTO
188700920611     C                   END
188800920609     C* CAMPI DEL SUBFILE
188900920609     C                   Z-ADD     SAVNR2        NR2
189000060306
189100060306      * se tariffa particolare "d" imposto i dati di dft
189200140128      * + il prezzo recuperato dal file TIACB
189300060306      * e scrivo una riga
189400060306     c                   if        vidftc = 'd ' and *in01
189500060307     c                   eval      *in79 = *on
189600060306     c                   eval      vi2cts = 'IT'
189700060306     c                   eval      vi2sgl = 9999999
189800140128     c******             select
189900060306      * clienti Top o Direzionali
190000140128     c******             when      clpclv = 'T' or clpclv = 'D'
190100111130     c****               eval      vi2itr = 0,002
190200140128     c******             eval      vi2itr = 0,003
190300060306      * clienti A
190400140128     c******             when      clpclv = 'A'
190500111130     c****               eval      vi2itr = 0,003
190600140128     c******             eval      vi2itr = 0,004
190700060306      * clienti C o B
190800140128     c******             when      clpclv = 'C' or clpclv = 'B'
190900111130     c****               eval      vi2itr = 0,005
191000140128     c******             eval      vi2itr = 0,007
191100060314      * non trovo la categoria cliente imposto il massimo
191200140128     c******             other
191300111130     c****               eval      vi2itr = 0,005
191400140128     c******             eval      vi2itr = 0,007
191500140128     c******             endsl
191600140128
191700140128       //?Recupero la tariffa AC BASE dal file TIACB
191800140128     c     kACB          setll     TIACB01L
191900140128     c     CLPclv        reade     TIACB01L
192000140128     c                   if        %found(TIACB01L)
192100140128     c                   eval      VI2itr = ACBitr
192200140128     c                   ENDIF
192300140128
192400060705      * controllo se il cliente è presente sulla tabella della forzatura
192500060705      * AC BASE e recupero la tariffa dalla tabella
192600060705     c                   clear                   dsbs02
192700060705     c                   clear                   dfab
192800060705     c                   eval      t02mod = 'C'
192900060705     c                   eval      t02sif = knsif
193000060705     c                   eval      t02cod = 'FAB'
193100060705     c  n12              movel     vidcli        t02ke1
193200060705     c   12              movel     visksc        t02ke1
193300060705     c                   call      'TIBS02R'
193400060705     c                   parm                    kpjba
193500060705     c                   parm                    dsbs02
193600060705     c                   if        t02err = *blanks
193700060705     c                   eval      dfab = t02uni
193800060705     c                   endif
193900060705     c                   if        d§fabitr > 0
194000060705     c                   eval      vi2itr = d§fabitr
194100060705     c                   endif
194200060705
194300060306     c                   z-add     0,001         vi2min
194400060306     c                   z-add     0             vi2max
194500060306     c                   z-add     vi2itr        savitr
194600060306     c                   z-add     vi2itr        sa2itr
194700060306     c                   z-add     vi2min        savmin
194800060306     c                   z-add     0             savmax
194900060306     c                   z-add     0             vi2nrr
195000060306     c                   add       1             nr2
195100060306     c                   add       1             b
195200060306     c                   write     ta67s07
195300060306     c                   setoff                                       0336
195400060306     c                   endif
195500920609     C*
195600920625     C     B             DOWLT     9
195700920609     C                   MOVEL     *BLANKS       VI2CTS
195800920609     C                   MOVEL     *BLANKS       VI2DCT
195900940712     C                   Z-ADD     0             VI2SGL
196000940712     C                   Z-ADD     0             VI2ITR
196100940712     C                   Z-ADD     0             VI2MIN
196200940713     C                   Z-ADD     0             VI2MAX
196300920611     C                   MOVEL     ' '           VI2ATB
196400920609     C* 06 ON - TIPO CONSEGNA = I
196500920610     C   06              MOVEL     'S'           VI2AIN
196600920612     C                   MOVEL     *BLANKS       VI2ORP
196700940712     C                   Z-ADD     0             SAVITR
196800960520     C                   Z-ADD     0             SA2ITR
196900940712     C                   Z-ADD     0             SAVMIN
197000940713     C                   Z-ADD     0             SAVMAX
197100930219     C   06              MOVEL     'S'           SAVAIN
197200930219     C  N06              MOVEL     ' '           SAVAIN
197300920609     C                   Z-ADD     0             VI2NRR
197400941116     C*
197500920609     C                   ADD       1             NR2
197600920609     C                   ADD       1             B
197700941111     C                   WRITE     TA67S07
197800920709     C* 36 - LO UTILIZZO IN CASO DI ERRORE PER POSIZIONARE IL CURSORE
197900920709     C*      SUL CAMPO ERRATO DELLA RELATIVA RIGA DI DETTAGLIO
198000920619     C                   SETOFF                                       0336
198100920609     C                   END
198200920609     C*
198300920609     C                   Z-ADD     NR2           SAVNR2
198400920609     C                   Z-ADD     NR2           REC2
198500920609     C                   ENDSR
198600920611     C*
198700920611     C*--- ANNULLAMENTO TOTALE TARIFFA -------------------------------*
198800920611     C     ANNTAR        BEGSR
198900920709     C                   SETOFF                                       32
199000920611     C* ANNULLO DETTAGLIO
199100061011     C  n12KTPT          SETLL     TITPD000
199200061011     c   12ktpt          setll     tiopd000
199300920611     C     *IN32         DOWEQ     '0'
199400061011     C  n12KTPT          READE     TITPD000                               32
199500061011     c   12ktpt          reade     tiopd000                               32
199600940713     C  N32TPDATB        IFEQ      ' '
199700941109     C     *IN12         IFEQ      *ON
199800941109     C* OFFERTA
199900061011     C                   DELETE    tiopd000
200000941109     C                   ELSE
200100941109     C* TARIFFA
200200941109     C                   MOVEL     'A'           TPDATB
200300941109     C                   MOVEL     ' '           TPDFTR
200400040915     C                   Z-ADD     dateu8        TPDDTR
200500990614     C                   UPDATE    TITPD000
200600941109     C                   END
200700921217     C                   END
200800920611     C                   END
200900921130     C*
201000940713     C* ANNULLO TESTATA
201100061011     C  n12KTPT          CHAIN     TITPT000                           31
201200061011     c   12ktpt          chain     tiopt000                           31
201300940713     C     *IN31         IFEQ      *OFF
201400941109     C     *IN12         IFEQ      *ON
201500941109     C* OFFERTA
201600061011     C                   DELETE    tiopt000
201700941109     C                   ELSE
201800941109     C* TARIFFA
201900990615     C* DATA ULTIMA VARIAZIONE
202000990615     C                   Z-ADD     DATEU8        TPTDUV
202100941109     C                   MOVEL     'A'           TPTATB
202200941109     C                   MOVEL     ' '           TPTFTR
202300040915     C                   Z-ADD     dateu8        TPTDTR
202400990614     C                   UPDATE    TITPT000
202500030715      * mi salvo la tariffa particolare annullata
202600030715     c                   movel(p)  TPTftc        w_manftc
202700030715     c                   move      'A'           W_manftc
202800030715     c                   z-add     1             WW
202900030715     c     w_manftc      lookup    manftc                                 87
203000030715     c                   if        not %found
203100030715     c     '   '         lookup    manftc(ww)                             87
203200030715     c                   if        %found
203300030715     c                   movea     w_manftc      manftc(ww)
203400030715     c                   endif
203500030715     c                   endif
203600941109     C                   END
203700940713     C                   END
203800921130     C*
203900921217     C*
204000920618     C                   SETON                                        09
204100920611     C                   ENDSR
204200920609     C*
204300920609     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
204400920609     C     CONSFL        BEGSR
204500920619     C                   SETOFF                                       993304
204600140131     c                   setoff                                       7976
204700920609     C                   Z-ADD     1             NR2
204800031003     C* INDICATORE 62 -> RIMANE ACCESO SE NON CI SONO RECORD VALORIZZATI
204900031003     C*                  NEL SUBFILE
205000031003     C                   SETON                                        62
205100140131
205200140131     c                   If        vidftc = 'f '
205300140131     C* INDICATORE 88 -> RIMANE ACCESO SE NON CI SONO scaglioni > = 9999
205400140131     C*                  NEL SUBFILE  solo per il fuel
205500140131     C                   SETON                                        88
205600140131
205700140131     c                   else
205800140219     c                   setoff                                       88
205900140131     c                   Endif
206000920609     C*
206100941111     C     NR2           CHAIN     TA67S07                            33
206200941111     C*
206300941111    1C     *IN33         DOWEQ     '0'
206400920624     C   15              MOVEL     DUPCTS        VI2CTS
206500940712     C   16              Z-ADD     DUPSGL        VI2SGL
206600940712     C   17              Z-ADD     DUPITR        VI2ITR
206700940712     C   18              Z-ADD     DUPMIN        VI2MIN
206800940713     C   23              Z-ADD     DUPMAX        VI2MAX
206900920624     C   19              MOVEL     DUPAIN        VI2AIN
207000920616     C*
207100941111    2C     VI2CTS        IFNE      *BLANKS
207200940713     C     RECORD        ORNE      *BLANKS
207300940713     C     RECORD        ANDNE     *ZEROS
207400031003      *
207500031003     C                   SETOFF                                       62
207600941111     C*
207700920609     C* CONTROLLO SE RECORD DI NUOVA IMMISSIONE
207800941111    3C     VI2NRR        IFEQ      0
207900920609     C                   SETOFF                                       05
208000920827     C*
208100920827     C*** CODICE TASSAZIONE
208200920609     C* CODICE TASSAZIONE OBBLIGATORIO
208300941111    4C     VI2CTS        IFEQ      *BLANKS
208400920611     C                   SETON                                        4599
208500920611     C                   GOTO      ENDSFL
208600941111    4C                   END
208700010920     C* CODICE TASSAZIONE "EE" ERRATO
208800020328      * se tariffa DPD
208900010920    4C     VI2CTS        IFEQ      'EE'
209000020325     C     FLGNET        ANDEQ     'D'
209100011022     C                   SETON                                        741399
209200011022     C                   MOVEA     MSG(1)        VI2MSG
209300011022     C                   GOTO      ENDSFL
209400010920    4C                   ENDIF
209500020328
209600020328      * se tariffa italia non accetto codice tassazione 'EE'
209700020328     c                   if        parfie = 'I' and vi2cts = 'EE'
209800020328     c                   eval      *in74 = *on
209900020328     c                   eval      *in13 = *on
210000020328     c                   eval      *in99 = *on
210100020328     c                   eval      vi2msg = msg(1)
210200020328     c                   goto      endsfl
210300020328     c                   endif
210400020328      * se tariffa estera non accetto codice tassazione 'IT'
210500020328     c                   if        parfie = 'E' and vi2cts = 'IT'
210600020328     c                   eval      *in74 = *on
210700020328     c                   eval      *in13 = *on
210800020328     c                   eval      *in99 = *on
210900020328     c                   eval      vi2msg = msg(4)
211000020328     c                   goto      endsfl
211100020328     c                   endif
211200020328
211300941020     C* SE LA CONSEGNA PARTICOLARE IN GESTIONE RICHIEDE UN PARTICOLARE
211400941111     C*   CODICE DI TASSAZIONE E' NECESSARIO CHE SIA PRESENTE SOLO
211500941020     C*   QUELLO NELLE RIGHE DI DETTAGLIO
211600950119    4C     WCTS          IFNE      *BLANKS
211700950119     C     WCTE          ANDNE     *BLANKS
211800941123     C*
211900950119    5C     WCTS          IFNE      VI2CTS
212000950119     C     WCTE          ANDNE     VI2CTS
212100950119     C                   MOVEA     WCTS          MS2(49)
212200950119     C                   MOVEA     WCTE          MS2(56)
212300950119     C                   MOVEA     MS2           VI2MSG
212400950119     C                   SETON                                        741399
212500941020     C                   GOTO      ENDSFL
212600941123    5C                   ENDIF
212700941123     C*
212800941123   X4C                   ELSE
212900941123     C*
213000941123    5C     WCTS          IFNE      *BLANKS
213100941123     C     WCTS          ANDNE     VI2CTS
213200950119     C                   MOVEA     WCTS          MS1(50)
213300950119     C                   MOVEA     MS1           VI2MSG
213400950119     C                   SETON                                        741399
213500941123     C                   GOTO      ENDSFL
213600941123    5C                   ENDIF
213700950119     C*
213800950119    5C     WCTE          IFNE      *BLANKS
213900950119     C     WCTE          ANDNE     VI2CTS
214000950119     C                   MOVEA     WCTE          MS1(50)
214100950119     C                   MOVEA     MS1           VI2MSG
214200950119     C                   SETON                                        741399
214300950119     C                   GOTO      ENDSFL
214400950119    5C                   ENDIF
214500941123    4C                   ENDIF
214600120713
214700120713      * ricerca codice tassazione
214800120713     c                   IF        %scan('?':VI2cts) > 0
214900120713     c                   clear                   TRTB09DS
215000120713     c                   eval      TB09fun = '1'
215100120713     c                   eval      TB09ord = '1'
215200120713     c                   IF        flgnet = 'D'
215300120713     c                   eval      TB09est = 'E'
215400120713     c                   ELSE
215500120713     c                   eval      TB09est = parfie
215600120713     c                   ENDIF
215700120713     c                   eval      TB09uta = 'S'
215800120713     c                   IF        flgnet = 'F'
215900120713     c                   eval      TB09utf = 'S'
216000120713     c                   ELSE
216100120713     c                   eval      TB09utf = 'N'
216200120713     c                   ENDIF
216300120713     c                   eval      kpjbu = TRTB09DS
216400120713     c                   call      'TRTB09R'
216500120713     c                   parm                    kpjba
216600120713     c                   eval      TRTB09DS = kpjbu
216700120713     c                   IF        TB09cod <> *blanks
216800120713     c                   eval      VI2cts = TB09cod
216900120713     c                   ELSE
217000120723     c                   clear                   VI2cts
217100120713     c                   ENDIF
217200120713     c                   ENDIF
217300941123     C*
217400920609     C* SE IMMESSO COD.TASSAZIONE CONTROLLO SE ESISTE
217500920609     C                   MOVEL     'CT'          COD
217600920611     C                   MOVEL     *BLANKS       KEY
217700920609     C                   MOVEL     VI2CTS        KEY
217800920609     C     KTAB          CHAIN     TABEL                              34
217900920611     C* CONTROLLO VALIDITA' CODICE TASSAZIONE
218000941111    4C  N34TBLFLG        IFNE      *BLANKS
218100920611     C                   SETON                                        4699
218200920611     C                   GOTO      ENDSFL
218300941111   X4C                   ELSE
218400950112     C                   MOVEL     TBLUNI        DSCT
218500950112     C*
218600950112     C*
218700920611     C* DECODIFICO CODICE TASSAZIONE
218800920617     C                   MOVEL     §CTDES        VI2DCT
218900990614     C                   MOVE      §CTCOR        VI2ORP
219000941111    4C                   END
219100950112     C*
219200920611     C* 34 ON - CODICE TASSAZIONE INESISTENTE
219300920611     C   34              SETON                                        4799
219400920611     C   34              GOTO      ENDSFL
219500050304      * errore bloccante se codice tassazione non utilizzabile in gestione
219600050304      * tariffe clienti
219700050304     c                   If        §ctuta = 'N'
219800050304     c                   Eval      *In47 = *On
219900050304     c                   Eval      *In99 = *On
220000050304     c                   Goto      endsfl
220100050304     c                   EndIf
220200020207      *
220300020207      * Se è tariffa FedEx devo copiare solo i codici tassazioni
220400020207      * previsti per FedEx
220500020207     C     FLGNET        IFEQ      'F'
220600020207     C     §CTUTF        ANDNE     'S'
220700020207     C                   SETON                                        769913
220800020207     C                   MOVEA     MSG(2)        VI2MSG
220900020207     C                   GOTO      ENDSFL
221000020207     C                   ENDIF
221100020207      * Se NON è tariffa FedEx devo copiare solo i codici tassazioni
221200020207      * NON previsti per FedEx
221300020207     C     FLGNET        IFNE      'F'
221400020207     C     §CTUTF        ANDEQ     'S'
221500020207     C                   SETON                                        779913
221600020207     C                   MOVEA     MSG(3)        VI2MSG
221700020207     C                   GOTO      ENDSFL
221800020207     C                   ENDIF
221900070226      * Se tariffa estero accetto solo estero
222000070226     c                   if        parfie = 'E' and §ctest = *Blanks
222100070226     c                   eval      *in13 = *on
222200070226     c                   eval      *in76 = *on
222300070226     c                   eval      *in99 = *on
222400070226     c                   movea     msg(12)       vi2msg
222500070226     c                   goto      endsfl
222600070226     c                   endif
222700070226      * Se tariffa DPD accetto solo estero
222800070226      * tranne il codice generico IT xchè va bene
222900070226     c                   if        flgnet = 'D' and §ctest = *Blanks and
223000070227     c                             vi2cts <> 'IT'
223100070226     c                   eval      *in13 = *on
223200070226     c                   eval      *in76 = *on
223300070226     c                   eval      *in99 = *on
223400070226     c                   movea     msg(12)       vi2msg
223500070226     c                   goto      endsfl
223600070226     c                   endif
223700070226      * Se tariffa italia accetto solo italia
223800070226     c                   if        parfie = 'I' and flgnet <> 'D' and
223900070226     c                                              §ctest <> *Blanks
224000070226     c                   eval      *in13 = *on
224100070226     c                   eval      *in76 = *on
224200070226     c                   eval      *in99 = *on
224300070226     c                   movea     msg(12)       vi2msg
224400070226     c                   goto      endsfl
224500070226     c                   endif
224600050704
224700050704      * Se tariffa particolare a spedizione devo accettare solo 1 scaglione
224800050704      * per codice di tassazione
224900050704     c                   If        vi2tpg = '2' and vi2atb = *Blanks
225000061011     c  n12ktpd01        Setll     titpd01l
225100061011     c   12ktpd01        Setll     tiopd01l
225200050704     c                   Do        *Hival
225300061011     c  n12ktpd01        Reade(n)  titpd01l
225400061011     c   12ktpd01        Reade(n)  tiopd01l
225500061011     c                   If        %eof
225600050704     c                   Leave
225700050704     c                   EndIf
225800050704     c                   If        tpdatb <> *Blanks
225900050704     c                   Iter
226000050704     c                   EndIf
226100050704     c                   Eval      *in74 = *on
226200050704     c                   Eval      *in13 = *on
226300050704     c                   Eval      *in99 = *on
226400050704     c                   Eval      vi2msg = msg(6)
226500050704     c                   Goto      endsfl
226600050704     c                   EndDo
226700050704     c                   EndIf
226800020207      *
226900920827     C*
227000920827     C*** SCAGLIONE OBBLIGATORIO
227100941111    4C     VI2SGL        IFEQ      0
227200920609     C                   SETON                                        4899
227300920609     C                   GOTO      ENDSFL
227400950119   X4C                   ELSE
227500990622     C* SE TIPO TARIFFA/PAGAMENTO E' A VALORE LO SCAGLIONE PUO' ESSERE UN VALORE
227600990622     C* ANCHE CON I DECIMALI IN BASE ALLA DIVISA
227700990622    5C     *IN14         IFEQ      *ON
227800990622     C                   MOVE      VI2SGL        W0030
227900990622    6C     W0030         IFGT      0
228000990622     C     FLGDEC        ANDNE     'S'
228100990622     C                   SETON                                        7199
228200990622     C                   GOTO      ENDSFL
228300990622    6C                   ENDIF
228400990622     C**
228500990622   X5C                   ELSE
228600950119     C* WSCD = " " --> SI TRATTA DI SCAGLIONE SENZA DECIMALI QUINDI NON
228700950119     C*                SI POSSONO INSERIRE VALORI DOPO LA VIRGOLA
228800990622    6C     WSCD          IFEQ      ' '
228900950119     C                   MOVE      VI2SGL        W0030
229000950119     C     W0030         IFGT      0
229100950119     C                   SETON                                        7199
229200950119     C                   GOTO      ENDSFL
229300950119     C                   ENDIF
229400990622    6C                   ENDIF
229500990622    5C                   ENDIF
229600080922    4C                   END
229700920827     C*
229800920827     C*** CONTROLLO SE RIGHE DOPPIE
229900061011     C  n12KTPD          CHAIN     TITPD000                           32
230000061011     c   12ktpd          chain     tiopd000                           32
230100061011     c  n32
230200061011    4CanN12TPDNRR        IFNE      VI2NRR
230300941111     C     TPDATB        ANDEQ     ' '
230400920709     C                   SETON                                        4999
230500920709     C                   GOTO      ENDSFL
230600941111    4C                   END
230700061011     c  n32
230800061011    4can 12tponrr        ifne      VI2NRR
230900061011     C     TPDATB        ANDEQ     ' '
231000061011     C                   SETON                                        4999
231100061011     C                   GOTO      ENDSFL
231200061011    4C                   END
231300920828     C*
231400941111   X3C                   ELSE
231500920617     C                   SETON                                        05
231600941111    3C                   END
231700060307
231800060307      * se tariffa particolare 'd ' AC BASE non posso andare al di sotto del
231900060307      * minimo indicato in DV 1463
232000060705      * o in tabella FAB
232100060313     c                   If        vidftc = 'd '
232200060705      * controllo se il cliente è presente sulla tabella della forzatura
232300060705      * AC BASE e controllo la tariffa inserita con quanto presente in tabella
232400060705     c                   clear                   dsbs02
232500060705     c                   clear                   dfab
232600060705     c                   eval      t02mod = 'C'
232700060705     c                   eval      t02sif = knsif
232800060705     c                   eval      t02cod = 'FAB'
232900060705     c  n12              movel     vidcli        t02ke1
233000060705     c   12              movel     visksc        t02ke1
233100060705     c                   call      'TIBS02R'
233200060705     c                   parm                    kpjba
233300060705     c                   parm                    dsbs02
233400060705     c                   if        t02err = *blanks
233500060705     c                   eval      dfab = t02uni
233600060705     c                   endif
233700060705     c                   if        vi2itr < d§fabitr
233800060705     c                   eval      vi2msg = msg(11)
233900060705     c                   eval      %subst(vi2msg:38:12) = %editc(d§fabitr:'4')
234000060705     c                   eval      *in13 = *on
234100060705     c                   eval      *in79 = *on
234200060705     c                   eval      *in99 = *on
234300060705     c                   goto      endsfl
234400060705     c                   endif
234500060705      * se cliente presente in tabella FAB non faccio gli altri controlli
234600060705     c                   if        d§fabitr = *zeros
234700140128
234800140128       //?Recupero la tariffa AC BASE dal file TIACB
234900140128     c     kACB          setll     TIACB01L
235000140128     c     CLPclv        reade     TIACB01L
235100140128     c                   IF        %found(TIACB01L) and
235200140128     c                             VI2itr < ACBitr
235300140128     c                   eval      vi2msg = msg(7)
235400140128     c                   eval      %subst(vi2msg:10:1) = clpclv
235500140128     c                   eval      VI2msg = %trim(VI2msg) + ' ' +
235600140128     c                             %trim(%editc(ACBitr:'4'))
235700140128     c                   eval      *in13 = *on
235800140128     c                   eval      *in79 = *on
235900140128     c                   eval      *in99 = *on
236000140128     c                   goto      endsfl
236100140128     c                   ENDIF
236200140128
236300140128     c******             Select
236400060307      * clienti Top o Direzionali
236500140128     c******             when      (clpclv = 'T' or clpclv = 'D') and
236600140128     c******                       vi2itr < 0,003
236700111130     c****                         vi2itr < 0,002
236800140128     c******             eval      vi2msg = msg(7)
236900111130     c****               eval      %subst(vi2msg:50:5) = '0,002'
237000140128     c******             eval      %subst(vi2msg:50:5) = '0,003'
237100140128     c******             eval      %subst(vi2msg:10:1) = clpclv
237200140128     c******             eval      *in13 = *on
237300140128     c******             eval      *in79 = *on
237400140128     c******             eval      *in99 = *on
237500140128     c******             goto      endsfl
237600060307      * clienti A
237700140128     c******             when      clpclv = 'A' and
237800140128     c******                       vi2itr < 0,004
237900111130     c****                         vi2itr < 0,003
238000140128     c******             eval      vi2msg = msg(7)
238100111130     c****               eval      %subst(vi2msg:50:5) = '0,003'
238200140128     c******             eval      %subst(vi2msg:50:5) = '0,004'
238300140128     c******             eval      %subst(vi2msg:10:1) = clpclv
238400140128     c******             eval      *in13 = *on
238500140128     c******             eval      *in79 = *on
238600140128     c******             eval      *in99 = *on
238700140128     c******             goto      endsfl
238800060307      * clienti C o B
238900140128     c******             when      (clpclv = 'C' or clpclv = 'B') and
239000140128     c******                       vi2itr < 0,007
239100111130     c****                         vi2itr < 0,005
239200140128     c******             eval      vi2msg = msg(7)
239300111130     c****               eval      %subst(vi2msg:50:5) = '0,005'
239400140128     c******             eval      %subst(vi2msg:50:5) = '0,007'
239500140128     c******             eval      %subst(vi2msg:10:1) = clpclv
239600140128     c******             eval      *in13 = *on
239700140128     c******             eval      *in79 = *on
239800140128     c******             eval      *in99 = *on
239900140128     c******             goto      endsfl
240000140128     c******             endsl
240100060705     c                   endif
240200060313     c                   endif
240300140115
240400140131      //?Se tariffa particolare 'f ' FUEL se importo > 0 o scaglione >=9999
240500140131      //?spendo INDICATORI DI ERRORE
240600140131     c                   IF        VIDftc = 'f ' and
240700140131     c                             VI2sgl >= 9999
240800140220     c                             and  VI2atb = ' '
240900140131     c                   setoff                                       88
241000140131     c                   ENDIF
241100140115
241200920827     C*
241300930219     C*** PER TUTTE CONSEGNE PARTICOLARI SI PUO' ACCETTARE
241400921202     C*     L'IMPORTO TARIFFA = 0  CON IL CMD14 DI FORZATURA
241500941111    3C     VI2ITR        IFEQ      0
241600930219     C  NKN              SETON                                        6199
241700921202     C   99              GOTO      ENDSFL
241800960520     C*
241900941111   X3C                   ELSE
242000960520     C* CONTROLLO CHE NON SUPERI LA MASSIMA % PER LA TARIFFA A VALORE:
242100960520     C*   SE LA SUPERA EMETTO MESSAGGIO FORZABILE
242200941111    4C     *IN14         IFEQ      *ON
242300960520     C*
242400960704    5C     VI2ITR        IFNE      SA2ITR
242500960520     C     VI2ITR        ANDGT     WPTV
242600941110     C                   SETON                                        6699
242700960704     C                   MOVEL     VI2ITR        SA2ITR
242800941110     C                   GOTO      ENDSFL
242900941111    5C                   END
243000941111     C*
243100941111   X4C                   ELSE
243200941110     C* SE SI TRATTA DI TARIFFA IN £ NON SI POSSONO INSERIRE I DECIMALI
243300941111     C                   MOVE      VI2ITR        W0030             3 0
243400941111    5C     W0030         IFGT      0
243500990622     C     FLGDEC        ANDNE     'S'
243600941110     C                   SETON                                        6899
243700941110     C                   GOTO      ENDSFL
243800941111    5C                   END
243900941111    4C                   END
244000941111    3C                   END
244100990622     C*
244200990622     C*** IMPORTO MINIMO  TARIFFA
244300990622     C* CONTROLLO I DECIMALI
244400990622    3C     VI2MIN        IFGT      0
244500990622     C                   MOVE      VI2MIN        W0030             3 0
244600990622    4C     W0030         IFGT      0
244700990622     C     FLGDEC        ANDNE     'S'
244800990622     C                   SETON                                        6499
244900990622     C                   GOTO      ENDSFL
245000990622    4C                   END
245100990622    3C                   END
245200990622     C*
245300990622     C*** IMPORTO MASSIMO TARIFFA
245400990622     C* CONTROLLO I DECIMALI
245500990622    3C     VI2MAX        IFGT      0
245600990622     C                   MOVE      VI2MAX        W0030             3 0
245700990622    4C     W0030         IFGT      0
245800990622     C     FLGDEC        ANDNE     'S'
245900990622     C                   SETON                                        6599
246000990622     C                   GOTO      ENDSFL
246100990622    4C                   END
246200990622    3C                   END
246300920827     C*
246400940713     C*** IMPORTO MASSIMO TARIFFA
246500940713     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
246600941111    3C     VI2MAX        IFGT      0
246700940713     C     VI2MAX        ANDLT     VI2MIN
246800940713     C                   SETON                                        6399
246900940713     C                   GOTO      ENDSFL
247000941111    3C                   END
247100940713     C*
247200920616     C* SE NON CI SONO ERRORI AGGIORNO E VADO AVANTI A LEGGERE
247300920616     C                   MOVEL     VI2CTS        DUPCTS
247400940712     C                   Z-ADD     VI2SGL        DUPSGL
247500941110     C                   Z-ADD     VI2ITR        DUPITR
247600940712     C                   Z-ADD     VI2MIN        DUPMIN
247700940713     C                   Z-ADD     VI2MAX        DUPMAX
247800920616     C   06              MOVEL     VI2AIN        DUPAIN
247900941111     C*
248000941111   X2C                   ELSE
248100930225     C                   SETOFF                                       05
248200941111    2C                   END
248300920616     C*
248400920611     C     ENDSFL        TAG
248500920610     C   99              Z-ADD     NR2           REC2
248600920619     C*
248700941111     C                   UPDATE    TA67S07
248800920619     C   99              SETON                                        33
248900920619     C*
249000921126     C                   ADD       1             NR2
249100941111     C  N99NR2           CHAIN     TA67S07                            33
249200941111    1C                   END
249300031003      * SE E' ACCESO L'INDICATORE 62 SEGNALO ERRORE CHE E' OBBLIGATORIO IL
249400031003      * DETTAGLIO TARIFFA
249500140219     C                   IF        *IN62 or *in88
249600031003     C                   SETON                                        99
249700031003     C                   ENDIF
249800920611     C                   ENDSR
249900920610     C*
250000920611     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
250100920611     C     CONCTL        BEGSR
250200060802     C                   SETOFF                                       999886
250300921126     C* AZZERO CAMPI DI DUP
250400940712     C                   MOVEL     *BLANKS       DUPCTS
250500940712     C                   Z-ADD     0             DUPSGL
250600941110     C                   Z-ADD     0             DUPITR
250700940712     C                   Z-ADD     0             DUPMIN
250800940713     C                   Z-ADD     0             DUPMAX
250900940712     C   06              MOVEL     *BLANKS       DUPAIN
251000060307
251100060307      * faccio subito i controlli per AC BASE e AC PLUS
251200060307
251300060307      * se tariffa particolare "d " AC BASE la tariffa non deve
251400060307      * avere il flag di rinuncia AC BASE
251500060307     c                   if        vidftc = 'd ' and parfmp = 'S'
251600060307     c                   eval      vi2msg = msg(8)
251700060307     c                   eval      *in13 = *on
251800060307     c                   eval      *in99 = *on
251900060307     c                   leavesr
252000060307     c                   endif
252100060307
252200060307      * se tariffa particolare "d " AC BASE la tariffa non deve
252300060307      * avere una tariffa particolare AC PLUS reale con applicazione completa
252400060307     c                   if        vidftc = 'd '
252500060307     c                   movel     'C '          v09ftc
252600061011     c  n12ktpt9         chain(n)  titpt01l
252700061011     c   12ktpt9         chain(n)  tiopt01l
252800061011     c                   if        %found and tptatb = *blanks and
252900060307     c                             tpttma = *blanks and tptapl = *blanks
253000060307     c                   eval      vi2msg = msg(9)
253100060307     c                   eval      *in13 = *on
253200060307     c                   eval      *in99 = *on
253300060307     c                   leavesr
253400060307     c                   endif
253500060307     c                   endif
253600060307
253700060307      * se tariffa particolare "C " AC PLUS reale completa la
253800060307      * tariffa non deve avere una tariffa particolare AC BASE
253900060307     c                   if        vidftc = 'C ' and vi2mas = *blanks and
254000060307     c                             vi2apl = *blanks
254100060307     c                   movel     'd '          v09ftc
254200061011     c  n12ktpt9         chain(n)  titpt01l
254300061011     c   12ktpt9         chain(n)  tiopt01l
254400061011     c                   if        %found and tptatb = *blanks
254500060307     c                   eval      vi2msg = msg(10)
254600060307     c                   eval      *in13 = *on
254700060307     c                   eval      *in99 = *on
254800060307     c                   leavesr
254900060307     c                   endif
255000060307     c                   endif
255100920827     C*
255200920924     C*** ARROTONDAMENTI : TUTTI <> 0  OPPURE  TUTTI = 0
255300941111    1C     VI2ARL        IFEQ      0
255400920611     C*
255500941111    2C     VI2ARF        IFNE      0
255600940712     C     VI2ARO        ORNE      0
255700920611     C                   SETON                                        4299
255800920611     C                   GOTO      ENDCON
255900941111    2C                   END
256000920611     C*
256100941111   X1C                   ELSE
256200940712     C*
256300941111    2C     VI2ARF        IFEQ      0
256400940712     C     VI2ARO        OREQ      0
256500920611     C                   SETON                                        4299
256600920611     C                   GOTO      ENDCON
256700941111    2C                   END
256800920611     C*
256900941111    1C                   END
257000920624     C*
257100950119     C* SE SONO STATI IMMESSI GLI ARROTONDAMENTI CONTROLLO SE SI TRATTA
257200950119     C*   DI SCAGLIONE SENZA DECIMALI PER CUI NON SI POSSONO INSERIRE
257300950119     C*   VALORI DOPO LA VIRGOLA
257400950119    1C     VI2ARL        IFGT      0
257500950119     C     WSCD          ANDEQ     ' '
257600950119     C                   MOVE      VI2ARL        W0030
257700950119    2C     W0030         IFGT      0
257800950119     C                   SETON                                        7299
257900950119     C                   GOTO      ENDCON
258000950119    2C                   ENDIF
258100950119     C*
258200950119     C                   MOVE      VI2ARF        W0030
258300950119    2C     W0030         IFGT      0
258400950119     C                   SETON                                        7299
258500950119     C                   GOTO      ENDCON
258600950119    2C                   ENDIF
258700950119     C*
258800950119     C                   MOVE      VI2ARO        W0030
258900950119    2C     W0030         IFGT      0
259000950119     C                   SETON                                        7299
259100950119     C                   GOTO      ENDCON
259200950119    2C                   ENDIF
259300950119    1C                   ENDIF
259400950119     C*
259500941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
259600941111    1C     *IN11         IFEQ      '1'
259700930223     C*
259800930223     C*** VALORE MERCE FORZABILE A ZERO CON ENTER
259900980309     C* SOLO SE NON SI TRATTA DI UNA TARIFFA IMPORT
260000990217    2C     *IN43         IFEQ      *ON
260100980429     C** SE SI TRATTA DI UNA MANDATO ASSICURATIVO FITTIZIO VI2MAS = N
260200980429     C** IL VALORE MERCE ASSICURABILE DEVE ESSERE A ZERO
260300981126    3C     VI2MAS        IFEQ      'F'
260400980429     C     VI2VLM        ANDGT     0
260500980429     C                   SETON                                        5299
260600980429     C                   GOTO      ENDCON
260700981126    3C                   END
260800981126    2C                   END
260900980309     C*
261000981126    2C     IMPORT        IFNE      'Y'
261100980416     C* CONTROLLO SE TARIFFA ESTERA
261200981126     C   24
261300981126    3COR 25PARFIE        IFEQ      'E'
261400980416     C* SE IL VALORE MERCE E' A ZERO IL TIPO APPLICAZIONE DEVE ESSERE SOLO
261500980416     C* IN PARTENZA
261600980416     C     VI2VLM        ANDEQ     0
261700980416     C     VI2APL        ANDNE     'P'
261800980416     C                   SETON                                        5399
261900980416     C                   GOTO      ENDCON
262000981126    3C                   END
262100980416     C*
262200981126    3C   24UNO           IFEQ      ' '
262300930223     C     VI2VLM        ANDEQ     0
262400980505     C     VI2MAS        ANDEQ     ' '
262500930223     C                   SETON                                        5099
262600930223     C                   MOVEL     '1'           UNO
262700930223     C                   GOTO      ENDCON
262800981126    3C                   END
262900981126   X2C                   ELSE
263000980309     C* VALORE MERCE
263100981126    3C   24VI2VLM        IFEQ      *ZEROS
263200980309     C                   SETON                                        5599
263300981126    3C                   END
263400980309     C* APPLICAZIONE TARIFFA SOLO IN ARRIVO
263500981126     C   24
263600981126    3COR 25VI2APL        IFNE      'A'
263700980309     C                   SETON                                        5499
263800980416     C                   GOTO      ENDCON
263900981126    3C                   END
264000980309     C*
264100981126    2C                   END
264200990817     C* VALORE MERCE VERIFICO SE HA IMPORTI CON DECIMALI
264300990817     C*
264400990817    2C     VI2VLM        IFGT      0
264500990817     C                   MOVE      VI2VLM        W0030             3 0
264600990817    3C     W0030         IFGT      0
264700990817     C     FLGDEC        ANDNE     'S'
264800990817     C                   SETON                                        6799
264900990817     C                   GOTO      ENDCON
265000990817    3C                   END
265100990817    2C                   END
265200921009     C*
265300930219     C*** LIMITE RISARCIBILE NON PUO' ESSERE MINORE DI 12000
265400990907    2C   25VI2VLM        IFLT      §GELRP
265500930219     C                   SETON                                        5699
265600930219     C                   GOTO      ENDCON
265700941111    2C                   END
265800921009     C*
265900920827     C*** TIPO VALORE MERCE
266000920827     C* TIPO VALORE OBBLIGATORIO
266100941111    2C     VI2FVM        IFEQ      '?'
266200980316     C                   MOVEL     'TR'          COD
266300920925     C                   MOVEL     ' '           VI2FVM
266400001020     C                   MOVE      *BLANKS       KEY
266500920925     C                   MOVEL     *BLANKS       DES
266600920925     C                   CALL      'X§TABER'
266700920925     C                   PARM                    CODUT
266800920925     C                   PARM                    COD
266900920925     C                   PARM                    KEY
267000920925     C                   PARM                    DES
267100920925     C                   MOVEL     KEY           VI2FVM
267200920925     C                   MOVEL     DES           DESFVM
267300920925     C                   SETON                                        98
267400920925     C                   GOTO      ENDCON
267500941111    2C                   END
267600920925     C*
267700941111    2C     VI2FVM        IFEQ      ' '
267800920827     C                   SETON                                        5799
267900920827     C                   GOTO      ENDCON
268000941111    2C                   END
268100941111     C*
268200920827     C* CONTROLLO VALIDITA' TIPO VALORE
268300980316     C                   MOVEL     'TR'          COD
268400920827     C                   MOVEL     *BLANKS       KEY
268500920827     C                   MOVEL     VI2FVM        KEY
268600920827     C     KTAB          CHAIN     TABEL                              34
268700920827     C*
268800920827     C* SE TIPO VALORE ANNULLATO : ERRORE
268900941111    2C  N34TBLFLG        IFNE      ' '
269000920827     C                   SETON                                        5899
269100920827     C                   GOTO      ENDCON
269200941111   X2C                   ELSE
269300980316     C                   MOVEL     TBLUNI        DSTR
269400941111     C* CONTROLLO SE E' DA UTILIZZARE PER IL CALCOLO VALORE
269500980316     C     §TRUCV        IFNE      'S'
269600920827     C                   SETON                                        5999
269700920827     C                   GOTO      ENDCON
269800920827     C                   ELSE
269900920827     C* DECODIFICO TIPO VALORE
270000980316     C                   MOVEL     §TRDES        DESFVM
270100920827     C                   END
270200941111    2C                   END
270300920827     C*
270400920827     C* 34 ON - TIPO VALORE MERCE INESISTENTE
270500920827     C   34              SETON                                        5999
270600060802     C                   GOTO      ENDCON
270700941111    1C                   END
270800140219      * FUEL
270900060802      * 82 ON - controllo la data riferimento prezzo base
271000060802     c                   if        *in82
271100140219      * il prezzo non può essere a zero
271200140219     c                   if        vi2pmg = 0
271300140219     c                   eval      *in83 = *on
271400140219     c                   eval      *in99 = *on
271500060802     c                   goto      endcon
271600060802     c                   endif
271700140219      * verifico la correttezza del prezzo richiamndo il TRULPMG
271800140219     c                   clear                   trulpmgds
271900140219     c                   eval      ipmgpmg = vi2pmg
272000140219     c                   clear                   kpjbu
272100140219     c                   eval      kpjbu = Trulpmgds
272200140219     c                   call      'TRULPMGR'
272300140219     c                   parm                    kpjba
272400140219     c                   eval      trulpmgds = kpjbu
272500140219      * scaglione non trovato
272600140219     c                   If        opmgerr = '1'
272700140219     c                   eval      *in84 = *on
272800140219     c                   eval      *in99 = *on
272900140219     c                   eval      vo2sca = *zeros
273000140219     c                   eval      vo2dpb = *zeros
273100140219     c                   goto      endcon
273200140219     c                   endif
273300140219      * prezzo mai rilevato settimanalmente dal Ministero
273400140219     c                   If        opmgerr = '2'
273500140219     c                   eval      *in85 = *on
273600140219     c                   eval      *in99 = *on
273700140219     c                   eval      vo2sca = *zeros
273800140219     c                   eval      vo2dpb = *zeros
273900140219     c                   goto      endcon
274000140219     c                   endif
274100140219     c                   eval      vo2sca = opmgsca
274200140219     c                   eval      vo2dpb = opmgdtd
274300140219     c                   eval      vi2pmg = opmgpmg
274400060802
274500060802     c                   endif
274600920827     C*
274700920611     C     ENDCON        ENDSR
274800921009     C*
274900920624     C*--- AGGIORNO CAMPI DEL SFL ------------------------------------*
275000920610     C     REGIS         BEGSR
275100920709     C*
275200920610     C                   Z-ADD     1             NR2
275300941111     C     NR2           CHAIN     TA67S07                            31
275400920610     C     *IN31         DOWEQ     '0'
275500920616     C     VI2CTS        IFNE      *BLANKS
275600921126     C*
275700921126     C                   SETOFF                                       60
275800921126     C* VI2NRR = 0  SI TRATTA DI RECORD DI NUOVA IMMISSIONE
275900921126     C     VI2NRR        IFEQ      0
276000921216     C                   SETON                                        60
276100921126     C*
276200921126     C                   ELSE
276300921126     C     VI2ATB        IFEQ      ' '
276400921126     C     VI2REC        IFNE      SAVREC
276500921216     C                   SETON                                        60
276600921126     C                   END
276700921126     C                   ELSE
276800921126     C                   SETON                                        60
276900921126     C                   END
277000921126     C                   END
277100921126     C*
277200061011     C   60
277300061011     Cann12KTPD          CHAIN     TITPD000                           33
277400061011     c   60
277500061011     can 12ktpd          chain     tiopd000                           33
277600921126     C   60VI2ATB        IFNE      ' '
277700030715     c                   if        *in33 = *off
277800030715      * non è offerta
277900030715     c                   if        *in12 = *off
278000030715     C                   MOVEL     'A'           TPDATB
278100030715     C                   MOVEL     ' '           TPDFTR
278200040915     C                   Z-ADD     dateu8        TPDDTR
278300030715     C                   UPDATE    TITPD000
278400030716      * verifico prima se esiste l'immissioine di questa tariffa
278500030716     c                   movel(p)  VIDftc        w_manftc
278600030716     c                   move      'I'           W_manftc
278700030716     c                   z-add     1             WW
278800030716     c     w_manftc      lookup    manftc                                 87
278900030715      * mi salvo la tariffa particolare da registrare nello storico variazioni
279000030716      * se non esiste la fase di immissione
279100030716     c                   if        not %found
279200030716      *
279300030715     c                   movel(p)  VIDftc        w_manftc
279400030715     c                   move      'M'           W_manftc
279500030715     c                   z-add     1             WW
279600030715     c     w_manftc      lookup    manftc                                 87
279700030715     c                   if        not %found
279800030715     c     '   '         lookup    manftc(ww)                             87
279900030715     c                   if        %found
280000030715     c                   movea     w_manftc      manftc(ww)
280100030715     c                   endif
280200030715     c                   endif
280300030716      *
280400030716     c                   endif
280500030715      *
280600030715     c                   else
280700030715      * offerta
280800061011     C                   DELETE    tiopd000
280900030715     c                   endif
281000921218     C*
281100030715     c                   endif
281200030715     C*
281300990615     C* AGGANCIO TNTPT PER AGGIORNARE ULTIMA DATA VARIAZIONE CON TRASMISSIONE
281400990615     C* DEL DATO
281500061011     C  n12KTPT          CHAIN     TITPT000                           32
281600061011     c   12ktpt          chain     tiopt000                           32
281700990615     C  N32              CLEAR                   TPTFTR
281800990615     C* DATA ULTIMA VARIAZIONE
281900990615     C  N32              Z-ADD     DATEU8        TPTDUV
282000040915     C  N32              z-add     dateu8        TPTDTR
282100061011     C  N32
282200061011     Cann12              UPDATE    TITPT000
282300061011     c  n32
282400061011     can 12              update    tiopt000
282500990615     C*
282600921218     C*
282700920610     C                   ELSE
282800920611     C* 33 ON  - IMMISSIONE
282900920611     C* 33 OFF - VARIAZIONE
283000941110     C                   Z-ADD     VI2ITR        TPDITR
283100940712     C                   Z-ADD     VI2MIN        TPDMIN
283200940713     C                   Z-ADD     VI2MAX        TPDMAX
283300930217     C*
283400930219     C   06VI2AIN        IFEQ      'N'
283500930219     C                   MOVEL     ' '           TPDAIN
283600930219     C                   ELSE
283700930219     C                   MOVEL     VI2AIN        TPDAIN
283800930219     C                   END
283900930217     C*
284000920611     C                   MOVEL     VI2ATB        TPDATB
284100920617     C                   MOVEL     VI2ORP        TPDORP
284200930217     C  N06              MOVEL     ' '           TPDAIN
284300921214     C                   MOVEL     ' '           TPDFTR
284400920611     C* SOLO PER IMMISSIONE
284500920611     C   33              DO
284600920611     C                   MOVEL     VI2CTS        TPDCTS
284700940712     C                   Z-ADD     VI2SGL        TPDSGL
284800920612     C                   Z-ADD     VIDCLI        TPDKSC
284900920612     C                   Z-ADD     VIDCTR        TPDCTR
285000920612     C                   Z-ADD     VIDPRG        TPDPRG
285100920612     C                   MOVEL     VIDFTC        TPDFTC
285200920610     C                   END
285300980220     C* VALORE POSITIVO NEI CAMPI
285400980220     C                   MLLZO     1             TPDSGL
285500980220     C                   MLLZO     1             TPDITR
285600980220     C                   MLLZO     1             TPDMIN
285700980220     C                   MLLZO     1             TPDMAX
285800920610     C*
285900040915     C                   Z-ADD     dateu8        TPDDTR
286000061011     c                   if        not *in12
286100061011     C  N33              UPDATE    TITPD000
286200061011     C   33              WRITE     TITPD000
286300061011     c                   endif
286400061011     c                   if        *in12
286500061011     c  n33              update    tiopd000
286600061011     c   33              write     tiopd000
286700061011     c                   endif
286800030715      *
286900921217     C*
287000990615     C* AGGANCIO TNTPT PER AGGIORNARE ULTIMA DATA VARIAZIONE CON TRASMISSIONE
287100990615     C* DEL DATO
287200061011     C  n12KTPT          CHAIN     TITPT000                           32
287300061011     c   12ktpt          chain     tiopt000                           32
287400030715     c                   if        *in32 = *off
287500030715     C                   CLEAR                   TPTFTR
287600040915     C                   Z-ADD     DATEU8        TPTDTR
287700990615     C* DATA ULTIMA VARIAZIONE
287800030715     C                   Z-ADD     DATEU8        TPTDUV
287900061011     C  n12              UPDATE    TITPT000
288000061011     c   12              update    tiopt000
288100030715      * mi salvo la tariffa particolare da registrare nello storico variazioni
288200030715      * se non sono in offerta
288300030715     c                   if        *in12 = *off
288400030715      *
288500030715     c                   movel(p)  VIDftc        w_manftc
288600030715     c                   move      'M'           W_manftc
288700030715     c                   z-add     1             WW
288800030715     c     w_manftc      lookup    manftc                                 87
288900030715     c                   if        not %found
289000030715     c     '   '         lookup    manftc(ww)                             87
289100030715     c                   if        %found
289200030715     c                   movea     w_manftc      manftc(ww)
289300030715     c                   endif
289400030715     c                   endif
289500030715     C*
289600030715     c                   endif
289700990615     C*
289800030715     c                   endif
289900921126     C*
290000920610     C                   END
290100920616     C                   END
290200920616     C                   ADD       1             NR2
290300941111     C     NR2           CHAIN     TA67S07                            31
290400920610     C                   END
290500920610     C                   ENDSR
290600920610     C*
290700920624     C*--- AGGIORNO CAMPI DEL SFLCTL ---------------------------------*
290800920624     C     REGIS2        BEGSR
290900920709     C*
291000920624     C* 32 ON  - IMMISSIONE
291100920624     C* 32 OFF - VARIAZIONE
291200061011     C  n12KTPT          CHAIN     TITPT000                           32
291300061011     c   12ktpt          chain     tiopt000                           32
291400920624     C                   MOVEL     VI2TPG        TPTTPG
291500940712     C                   Z-ADD     VI2ARL        TPTARL
291600920624     C                   Z-ADD     VI2ARF        TPTARF
291700920624     C                   Z-ADD     VI2ARO        TPTARO
291800920624     C                   Z-ADD     VI2RPV        TPTRPV
291900920709     C                   MOVEL     ' '           TPTATB
292000990915     C                   MOVEL     VI2MAS        TPTTMA
292100060802     c                   If        *in82
292200140219     C                   Z-ADD     vo2dpb        TPTDPB
292300140115     c                   eval      §TPTpma = VI2pma
292400060829     c                   else
292500060829     c                   clear                   tptdpb
292600140115     c                   clear                   §TPTpma
292700060803     c                   endif
292800060803      *
292900060803     c   32              z-add     dateu8        TPTdit
293000941111     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
293100941111    1C     *IN11         IFEQ      *ON
293200941111     C                   Z-ADD     VI2VLM        TPTVLM
293300941111     C                   MOVEL     VI2FVM        TPTFVM
293400941111   X1C                   ELSE
293500941111     C                   Z-ADD     0             TPTVLM
293600941111     C                   MOVEL     ' '           TPTFVM
293700941111    1C                   ENDIF
293800941111     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
293900941111    1C     *IN28         IFEQ      *ON
294000941111     C                   MOVEL     VI2APL        TPTAPL
294100941111   X1C                   ELSE
294200941111     C                   MOVEL     ' '           TPTAPL
294300941111    1C                   ENDIF
294400941111     C*
294500920624     C* SOLO PER IMMISSIONE
294600941111    1C   32              DO
294700920624     C                   Z-ADD     VIDCLI        TPTKSC
294800920624     C                   Z-ADD     VIDCTR        TPTCTR
294900920624     C                   Z-ADD     VIDPRG        TPTPRG
295000920624     C                   MOVEL     VIDFTC        TPTFTC
295100941111    1C                   END
295200941111     C*
295300990614     C* SE VIENE VARIATO TITPT ABBLENCO FLAG E DATA TRASMISSIONE
295400941111    1C     COMTPT        IFNE      TPTDS
295500921215     C                   MOVEL     ' '           TPTFTR
295600040915     C                   Z-ADD     dateu8        TPTDTR
295700921217     C*
295800990903     C*
295900990615     C* DATA ULTIMA VARIAZIONE
296000990615     C                   Z-ADD     DATEU8        TPTDUV
296100941111    1C                   END
296200980220     C* VALORE POSITIVO NEI CAMPI
296300980220     C                   MLLZO     1             TPTARL
296400980220     C                   MLLZO     1             TPTARF
296500980220     C                   MLLZO     1             TPTARO
296600980220     C                   MLLZO     1             TPTRPV
296700980220     C                   MLLZO     1             TPTVLM
296800921215     C*
296900040915     C                   Z-ADD     dateu8        TPTDTR
297000140115     c                   eval      TPTflo = dTPT01
297100061011     c                   if        not *in12
297200990614     C  N32              UPDATE    TITPT000
297300990614     C   32              WRITE     TITPT000
297400061011     c                   endif
297500061011     c                   if        *in12
297600061011     c  n32              update    tiopt000
297700061011     c   32              write     tiopt000
297800061011     c                   endif
297900030715      *
298000030715      * se non sono in offerta salvo il codice tariffa particolare  da scrivere
298100030715      * nello storico
298200030715      *
298300030715     c                   IF        *in12 = *off
298400030715      *
298500030715     c                   movel(p)  VIDftc        w_manftc
298600030715     c  n32              move      'M'           W_manftc
298700030715     c   32              move      'I'           W_manftc
298800030715     c                   z-add     1             WW
298900030715     c     w_manftc      lookup    manftc                                 87
299000030715     c                   if        not %found
299100030715     c     '   '         lookup    manftc(ww)                             87
299200030715     c                   if        %found
299300030715     c                   movea     w_manftc      manftc(ww)
299400030715     c                   endif
299500030715     c                   endif
299600921126     C*
299700030715     c                   endif
299800030715     c*
299900930318     C*
300000930318     C                   SETON                                        0207
300100930318     C                   SETOFF                                       01
300200930223     C*
300300930223     C                   MOVEL     VI2TPG        SAVTPG            1
300400930223     C                   Z-ADD     VI2VLM        SAVVLM            9 0
300500920624     C                   ENDSR
300600131218     C*
300700131218     C*--- Recupero prezzo del gasolio corrispondente ----------------*
300800131218     C     REC_pmg       BEGSR
300900131218     C*
301000131218     c     Kpmg          setll     tipmg01l
301100131218     c                   read      tipmg01l
301200131218     c                   if        %found(tipmg01l)
301300131218      * devo controllare la data del prezzo medio che ho trovato
301400131218      * se la data del prezzo medio è troppo indietro non devo visualizzare
301500131218      * il prezzo (per ora faccio il controllo con 6 gg)
301600131218     c                   if        pmgdtd > *zeros
301700131218     c     *iso          move      pmgdtd        data_iso1
301800131218     c                   else
301900131218     c                   eval      data_iso1 = *loval
302000131218     c                   endif
302100131218     c     *iso          move      Kpmg          data_iso2
302200131218     c     data_iso2     subdur    data_iso1     wgio:*d
302300131218     c                   if        wgio > 6
302400140219     c                   clear                   vI2pmg
302500140219     c                   clear                   vO2sca
302600131218     c                   else
302700140219     c                   eval      vI2pmg = pmgpmg
302800140219     c                   eval      vO2sca = pmgsca
302900131218     c                   endif
303000131218     c                   endif
303100131218     C                   ENDSR
303200131218     C*
303300120320
303400141118     C*---------------------------------------------------------------*
303500141118     C*    Passaggio parametri al programma di calcolo FUEL           *
303600141118     C*---------------------------------------------------------------*
303700141118     C     Calc_fuel     BEGSR
303800141118     C*
303900141118     c                   clear                   sgl
304000141118     c                   clear                   itr
304100141118
304200141118     C                   Z-ADD     1             NR2
304300141118     C     NR2           CHAIN     TA67S07                            31
304400141118     C                   DOW       not *in31
304500141118     c                   if        savcts = *blanks
304600141118     c                   eval      savcts = VI2cts
304700141118     c                   endif
304800141118
304900141118     C                   IF        VI2cts = savcts
305000141118     c                   z-add     VI2sgl        sgl(nr2)
305100141118     c                   z-add     VI2itr        itr(nr2)
305200141118     c                   Endif
305300141118
305400141118     C                   ADD       1             NR2
305500141118     C     NR2           CHAIN     TA67S07                            31
305600141118     C                   END
305700141118
305800141118     c                   clear                   tnta42ds
305900141118     c                   eval      ita42sgl = sgl
306000141118     c                   eval      ita42itr = itr
306100141118     c                   eval      ita42pbe = VO2DPB
306200141118     c                   eval      ita42pma = VI2PMA
306300141118     c                   eval      ita42spb = VO2SCA
306400141118
306500141118     c                   call      'TNTA42R'
306600141118     c                   parm                    kpjba
306700141118     c                   parm                    tnta42ds
306800141118
306900141118     C                   ENDSR
307000141118     C*
307100120320      /free
307200120320       //--------------------------------------------------------------
307300120320       //?Visualizza Dati manca tariffa-
307400120320       //--------------------------------------------------------------
307500120320       BEGSR GesW01;
307600120320
307700120320         wFine = *off;
307800120320         W01dsp = ISB48dsp;
307900120320         W01tbl = ISB48tbl;
308000120320         W01lnp = ISB48lnp;
308100120320         W01lna = ISB48lna;
308200120320         W01pro = ISB48pro;
308300120320         W01cts = ISB48cts;
308400120320         W01ctr = ISB48ctr;
308500120320         W01ncl = ISB48ncl;
308600120320         W01pkf = ISB48pkf;
308700120320         W01vlf = ISB48vlf;
308800120320         W01err = ISB48err;
308900120320
309000120320         DOW wFine = *off;
309100120320           exfmt  TA67W01;
309200120320           //?- F12 = Ritorno
309300120320           IF  *inkl;
309400120320             wFine = *on;
309500120320           ENDIF;
309600120320         ENDDO;
309700120320
309800120320       ENDSR;
309900120320      /end-free
310000950119     C*---------------------------------------------------------------*
310100020207**  MS1
310200950119E' possibile inserire solo il Codice Tassazione "xx"                           1
310300950119**  MS2
310400950119E' possibile inserire solo i Codici Tassazione "xx" e "xx"                     1
310500011022**  MSG
310600050531Non è possibile inserire il Codice Tassazione "EE"                             1
310700050531Codice tassazione non utlizzabile per tariffa FedEx                            2
310800050531Codice tassazione utilizzabile solo per tariffe FedEx                          3
310900050531Non è possibile inserire il Codice Tassazione "IT"                             4
311000050531Impossibile inserire la Tar.Particolare "XX" che sarà eliminata il xx/xx/xx    5
311100050704Inserire solo uno scaglione per codice di tassazione!!                         6
311200140128Cliente " " l'importo non può essere inferiore a Euro                          7
311300060307Impossibile inserire AC BASE. Il cliente ha rinunciato al mandato ass. AC BASE 8
311400060307Impossibile inserire AC BASE. Presente AC PLUS reale con applicazione completa 9
311500060307Impossibile inserire AC PLUS reale con applicazione completa. Presente AC BASE 1
311600060705L'importo non può essere inferiore a xxxxxxxx,xxx Euro                        11
311700070226Codice tassazione non utilizzabile                                            12
311800140219                                                                              13
