000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300991013      * CNC0W4R                                                      *
000400911030      *        CHIUSURA PER OPERATORE                                *
000500940224?     *--------------------------------------------------------------*
000600940224      * 01 -ON--> PRIMO LIVELLO/OFF --> SECONDO LIVELLO              *
000700940224      * 02 -ON--> SIMFEL UGUALE A 0                                  *
000800941018      * 10 -ERRORE IN APERT, FILE SOTTO SINCRONIA                    *
000900940224      * 19 -INDICATORE GENERICO DI ERRORE                            *
001000940224      * 20 -ERRORE NEL RICHIAMO QCMDEXEC                             *
001100940224      * 21 -INDICATORE DI COMODO                                     *
001200940224      * 22 -INDICATORE DI COMODO                                     *
001300940224      * 23 -CHAIN SU FLARB31C                                        *
001400940224      * 24 -CLIENTI ESTERO                                           *
001500940224      * 25 -CLIENTI ITALIA                                           *
001600940224      * 26 -INDICATORE PER LOKUP                                     *
001700940224      * 30 -ERRMSG: ABBUONO FUORI LIMITI                             *
001800940224      * 31 -ERRMSG: TOTALE C/A INTESTAZIONE MITTENTE NON QUADRA      *
001900940224      * 32 -ERRMSG: NON TROVATE DISTINTE CHIUSE DA CONTABILIZZARE    *
002000941019      * 33 -ERRMSG: TOTALE C/A INTEST. MITT. IN VALUTA NON QUADRA    *
002100940224      * 35 -ERRMSG: NON E' POSSIBILE ESEGUIRE CHIUSURA PER CONFLITTI *
002200940224      *             E' IN ESECUZIONE LA CHIUSURA DI CASSA. ATTENDERE *
002300940224      *             QUALCHE MINUTO E PREMERE NUOVAMENTE CMD06        *
002400940224      * 36 -ERRMSG: NON E' POSSIBILE ESEGUIRE CHIUSURA PER CONFLITTI *
002500940224      *             UN ALTRO OPERATORE STA CHIUDENDO I SUOI INCASSI  *
002600940224      *             ATTENDERE QUALCHE MINUTO E RIPROVARE PREMENDO CMD6
002700940224      * 37 -ERRMSG: ATTENZIONE: LA PRECEDENTE CHIUSURA E' TERMINATA  *
002800940224      *             IN MODO ANOMALO. CONTROLLARE LE STAMPE DELLA PRE-*
002900940224      *             CEDNTE CHIUSURA                                  *
003000940224      * 38 -ERRMSG: NON E' POSSIBILE ESEGUIRE CHIUSURA PER CONFLITTI *
003100940224      *             UN ALTRO OPERATORE STA CHIUDENDO I SUOI INCASSI  *
003200940224      *             ATTENDERE QUALCHE MINUTO E RIPROVARE PREMENDO CMD6
003300940224      * 39 -ERRMSG: ATTENZIONE: LA PRECEDENTE CHIUSURA E' TERMINATA  *
003400940224      *             IN MODO ANOMALO. CONTROLLARE LE STAMPE DELLA PRE-*
003500940224      *             CEDNTE CHIUSURA                                  *
003600940224      * 41 -ABILITAZIONE CONFERMA                                    *
003700120424      * 51/91/95 -INDICATORE DI COMODO                   *
003800940224      * 71 -CONTRASSEGNO  + ASSEGNATO                                *
003900920623      * 77 - STAMPO DETTAGLIO BOLLE CHIUSURA OPERATORE               *
004000940224      * 80 - SPEDIZIONI CHIUSE CON RICHIESTA NOTE CREDITO            *
004100940224      * 88 - MANCA TABELLA                                           *
004200991014      * 98 - RIUSCITA ALLOCAZIONE FNMOV00T                           *
004300940224      * 99 - RIUSCITA ALLOCAZIONE FLARC00T                           *
004400900606?     *--------------------------------------------------------------*
004500991013     FCNC0W4D   CF   E             WORKSTN
004600060131     FFNblp01L  iF   E           K DISK
004700020919     FFNARB71L  UF   E           K DISK    USROPN
004800131212     F                                     infds(arbds)
004900930426     F                                     COMMIT
005000990311     FFNARB01L  UF   E           K DISK    USROPN
005100990311     F                                     RENAME(FNARB000:FNARBMAM)
005200990311     F                                     COMMIT
005300060328     FFNLBL01L  IF   E           K DISK
005400991011     FFIAR601L  IF   E           K DISK
005500991021     FFIARI01L  UF   E           K DISK    COMMIT
005600991021     F                                     USROPN
005700051114     FFIAR901L  IF   E           K DISK
005800120118     Ftncsm03L  IF   E           K DISK
005900941014     FCNASS01L  UF   E           K DISK    USROPN
006000930426     F                                     COMMIT
006100941014     FFNCAS00T  O    E             DISK    USROPN
006200930426     F                                     COMMIT
006300991021     FFNMOV00T  O    E             DISK    COMMIT
006400991021     F                                     USROPN
006500060131     FFIINC02L  uf   E           k DISK    COMMIT
006600060131     F                                     USROPN
006700060131     Fyfqig01l  IF   E           K DISK
006800021120     FTABEL00F  IF   E           K DISK
006900991011     FTNTBE01L  IF   E           K DISK
007000911031     FCNACO00F  IF   E           K DISK
007100911031     FAZORG01L  IF   E           K DISK
007200060130     Fyfmdi01L  O  A E           K DISK    USROPN commit
007300061023     Ffidpi01l  uf A E           K DISK    usropn
007400941017     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
007500941017     F                                     USROPN
007600011113     Fcnc0w4p   O    e             PRINTER oflind(*in07)
007700011114     F                                     USROPN
007800991013?     *--------------------------------------------------------------*
007900131212     d arbds           ds
008000131212     d  arb_nrr              397    400b 0
008100131212      *
008200011113     D DATISO          S               D                                        FLAGS ANAG
008300011113     D DATEUR          S               D   DATFMT(*EUR)                         FLAGS ANAG
008400011113     D OPZ             S              1    DIM(10)                              FLAGS ANAG
008500930409      *
008600930409     D T1A             S              2    DIM(20)                              TAB. TIPO
008700930409     D D1A             S             89    DIM(20)                              DS1A TIPO
008800941014     D T3A             S              2    DIM(40)                              TAB. COD.
008900930409     D D3A             S             89    DIM(40)                              DS3A TIPO
009000991018     D CCV             S              3    DIM(30)                              TAB. DIVIS
009100991018     D DCV             S             89    DIM(30)                              DSCV
009200930409      *
009300120424     D CMA             S             44    DIM(3) CTDATA PERRCD(1)              QCAEXEC AL
009400020920     D CMD1            S             48    DIM(2) CTDATA PERRCD(1)              QCAEXEC AD
009500020920     D CMD2            S             48    DIM(2) CTDATA PERRCD(1)              QCAEXEC OV
009600020920     D CMD3            S             48    DIM(2) CTDATA PERRCD(1)              QCAEXEC CH
009700991015      *
009800991015      * SCHIERA PER LA MEMORIZZAZIONE DELLE DIVISE
009900060328     d DIV             S              4    DIM(10)                              DIVISE X R
010000060328     D F6              C                   X'36'
010100060328     d totcas          S                   like(vinceu)                         DIVISE X R
010200060330     d comaasdpi       S                   like(dpiaas)                         DIVISE X R
010300060330     d comlnpdpi       S                   like(dpilnp)                         DIVISE X R
010400060330     d comnrsdpi       S                   like(dpinrs)                         DIVISE X R
010500060330     d comnspdpi       S                   like(dpinsp)                         DIVISE X R
010600060330     d comdivdpi       S                   like(dpidiv)                         DIVISE X R
010700060330     d comvaldpi       S                   like(dpivalore)                      DIVISE X R
010800100714     d savlna          S                   like(arblna)                         DIVISE X R
010900120123     d svasmeu         S                   like(vasmeu)                         DIVISE X R
011000120123     d svasmce         S                   like(vasmce)                         DIVISE X R
011100120123     d svmitva         S                   like(vmitva)                         DIVISE X R
011200120123     d svmivac         S                   like(vmivac)                         DIVISE X R
011300060328?     *--------------------------------------------------------------*
011400131212     d trul82ds      e ds
011500900515      *
011600020919     D ddatiute      e ds
011700020919     D azuteds       e ds                  extname(AZUTE00F)
011800020919     D tibs34ds      E DS                  inz
011900060214     D x17clvalds    E DS                  inz
012000020919     c* numeratori
012100020919     d trul33ds      e ds
012200020729     D Dy4o          E DS
012300020729     D DS1A          E DS
012400911127     D DS3A          E DS
012500941020     D DSCV          E DS
012600991011     D DGEI          E DS
012700991018     D DGED          E DS
012800060330     D cnc0wa00s     e ds                  prefix(£) inz
012900060330     D cnc0wb00s     e ds                  prefix($) inz
013000060330     D cnc0wc00s     e ds                  prefix(x) inz
013100900514     D KPJBA         E DS
013200900514     D  LIBSYS                92    100
013300900514     D  MSGERR               453    502
013400900516      *
013500900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
013600000000     D  TCU                  398    697
013700000000     D                                     DIM(50)                              TAB FLAG C
013800000000     D  KCU                  698    847P 0
013900000000     D                                     DIM(50)                              TAB CAPO C
014000000000     D                                     PACKEVEN
014100900606      *
014200911031     D TCUDS           DS
014300911031     D  F1                     1      1
014400911031     D  F3                     3      3
014500911031     D  F2                     2      2
014600911031     D  F4                     4      4
014700911031     D  F56                    5      6
014800911106     D PARAM           DS
014900911105     D  PFLE                   1      3
015000911106     D  PCUC                   4      6
015100911106     D  PNMI                   7     13
015200110914     D  PdMI                  14     21
015300920624     D*
015400941014     D WLBDA8          DS
015500941014     D  G08DAT                 1      8  0
015600941014     D  G08INV                 9     16  0
015700941014     D  G08ERR                17     17
015800941014     D  G08TGI                18     22  0
015900941014     D WGIDA8          DS
016000941014     D  GI8DAT                 1      8  0
016100941014     D  GI8INV                 9     16  0
016200941014     D  GI8TGI                17     21  0
016300020806
016400020828     d   mbr6          ds
016500020806     d  $emme                  1      1    inz('M')
016600020806     d  $simfel                2      4
016700020806     d  $mbr                   5      7    inz('046')
016800020806     d  $mbr6                  2      7
016900020806     d  $varmbr                1     10
017000020920     d   §membro       ds
017100020920     d  §emme                  1      1    inz('M')
017200020920     d  §fle                   2      4s 0
017300020920     d  §sed                   5      7s 0 inz(046)
017400020920     d  §mbr                   2      7
017500920624      *
017600941014     D                SDS
017700941014     D  §PGM                   1     10
017800021014     d* campi utili per richiamo pgm QCAPCMD (simile al QCMDEXC ma restitui-
017900021014     d* sce anche il codice d'errore)
018000021014     D pcmd01          s            256                                         Source cmd
018100021014     D pcmd02          s              9b 0 inz(%size(pcmd01))                   Length sou
018200021014     D pcmd03          s                   like(qcap0100)                       Options co
018300021014     D pcmd04          s              9b 0 inz(%size(pcmd03))                   Length opt
018400021014     D pcmd05          s              8    inz('CPOP0100')                      Opt. contr
018500021014     D pcmd06          s            256                                         Changed cm
018600021014     D pcmd07          s              9b 0 inz(%size(pcmd06))                   Length cha
018700021014     D pcmd08          s              9b 0                                      Length cha
018800021014
018900060214     D wcomo6          s              6  0                                      Length cha
019000060214     D subrig          s                   like(MOVSBR)                         Length cha
019100100517     D ptotcas         s             11s 2                                      Length cha
019200120314     d savrif          s             38
019300120314     d rifass          ds
019400120314     D §nra                                like(assnra)
019500120314     D §dte                                like(assdte)
019600120314     D §abi                                like(assabi)
019700120314     D §cab                                like(asscab)
019800120314     D §key                                like(assnra)
019900120314     D §vca            s                   like(assvca)
020000021014     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
020100021014     D/COPY QSYSINC/QRPGLESRC,QUSEC
020200021014     D qused01                      256
020300920624      *-------------------------------------------------------------------------
020400930617     C     *IN10         IFEQ      *ON                                          ERRORI IN
020500021120     C     *IN09         orEQ      *ON                                          ERRORI IN
020600991013     C                   EXFMT     CN0W401
020700930617     C                   GOTO      FINE
020800930617     C                   END
020900910521      *          =============
021000910521     C     FMT01         TAG
021100910521      *          =============
021200911205     C* CALCOLO GLI IMPORTI DALLE BOLLE
021300911205     C  N41              DO
021400920311     C                   SETOFF                                       80
021500911205     C                   EXSR      CALC
021600911205     C                   SETON                                        41
021700060329     c* prima di emettere il video imposto tutte le scelte in modo da
021800060329     c* richiamare in automatico le window degli incassi
021900060329     c                   if        vconeu <> 0
022000060331     c                   eval      selconv = 'X'
022100060329     c                   end
022200060329     c                   if        vasneu <> 0
022300060331     c                   eval      selasbv = 'X'
022400060329     c                   end
022500060329     c                   if        vpos <> 0
022600060331     c                   eval      selposv = 'X'
022700060329     c                   end
022800060329     c                   if        vasmce <> 0
022900060331     c                   eval      selamv = 'X'
023000060329     c                   end
023100060329     c                   if        vmivac <> 0
023200060331     c                   eval      selamvv = 'X'
023300060329     c                   end
023400911205     C                   END
023500910521      *
023600060328     c                   if        *in19
023700060328     c                   setoff                                       19
023800060328     C                   write     CN0W401
023900060328     c                   seton                                        19
024000060328     c                   end
024100060328      *
024200060328     C                   EXFMT     CN0W401
024300941018     C   KC              GOTO      FINE                                         F3=FINE
024400911205      *
024500920623      *Eventuale stampa bolle chiusura operatore CMD10
024600011113     c                   if        *inkj
024700011113     c                   exsr      stampa
024800011113     c                   end
024900920623      *
025000920623     C  NKJ              EXSR      CTR1
025100920623     C                   SETOFF                                       KJ
025200910521     C* ERRORI
025300920320     C   88                                                                     MANCA TAB.
025400941018     CORNKF
025500911104     COR 19              GOTO      FMT01
025600910521      *
025700920623      *
025800941018      * AGGIORNAMENTO  CMD06
025900941018     C   KF              DO
026000011113     C*  STAMPO LA CHIUSURA OPERATORE
026100011113     C                   EXSR      STAMPA
026200911031     C* PRIMA ALLOCO I FILE TRASMISSIONE  SE NON CI RIESCO MANDO MSG
026300011115     C                   EXSR      CTR1
026400911031     C   19              GOTO      FMT01
026500011115     C* PRIMA ALLOCO I FILE TRASMISSIONE  SE NON CI RIESCO MANDO MSG
026600011115     C                   EXSR      ALCOBJ
026700011115     C   19              GOTO      FMT01
026800020919     C*  PRENDO NR. MANDATO DI INTROITO
026900020919     c                   do        *hival
027000020919     C                   CLEAR                   TRUL33DS
027100020919     c                   eval      i33tla = 'L'
027200020919     C                   Z-ADD     13            I33CNU
027300020919     C                   Z-ADD     §FLE          I33PO1
027400020919     C                   Z-ADD     1             I33NUM
027500020919     c                   movel(p)  trul33ds      kpjbu
027600020919     c                   call      'TRUL33R'
027700020919     c                   parm                    kpjba
027800020919     c                   movel     kpjbu         trul33ds
027900020919     C                   IF        O33ERR = 0
028000020919     C                   Z-ADD     O33Nrf        §NMI
028100020919     c                   leave
028200020919     C                   END
028300020919     C                   ENDdo
028400991019     C*
028500911031     C*  STAMPO IL MANDATO DI INTROITO
028600911031     C                   EXCEPT    STANMI
028700911031     C*
028800991018     C*  SCRIVO FILE MANDATI D'INTROITO
028900991018     C                   EXSR      WRTARM
029000911031     C*  SCRIVO MOV. CONTAB. E AGGIORNO
029100911031     C                   EXSR      AGGIOR
029200991014     C                   CLOSE     FNMOV00T
029300941014     C                   CLOSE     FNCAS00T
029400911111     C*
029500911111     C* SOTTOMETTO TRASMISSIONE BOLLE ARRIVO CONSEGNATE
029600911111     C*
029700020919     C                   MOVEL     §NMI          NMI7A            10
029800020919     C                   MOVE      §FLE          NMI7A
029900020919     C                   MOVEL(P)  NMI7A         KPJBU
030000941014     C                   MOVEL     'LR90'        KCOAZ
030100911111     C                   CALL      'BCH10'
030200911111     C                   PARM                    KPJBA
030300941221     C* TRASMETTO DATI CON MANDATO DI INTROITO 999999
030400020919     C                   MOVEL     '9999999'     NMI7A
030500020925     C                   MOVE      §FLE          NMI7A
030600020919     C                   MOVEL(P)  NMI7A         KPJBU
030700941221     C                   MOVEL     'LR90'        KCOAZ
030800941221     C                   CALL      'BCH10'
030900941221     C                   PARM                    KPJBA
031000100714     C* TRASMETTO DATI CON MANDATO DI INTROITO 999999 di p.o. in £6
031100100714     c                   if        savlna <> 0
031200100714     C                   MOVEL     '9999999'     NMI7A
031300100714     C                   MOVE      savlna        NMI7A
031400100714     C                   MOVEL(P)  NMI7A         KPJBU
031500100714     C                   MOVEL     'LR90'        KCOAZ
031600100714     C                   CALL      'BCH10'
031700100714     C                   PARM                    KPJBA
031800100714     c                   end
031900920213     C*
032000970121     C* RICHIAMO TRASMISSIONE AR4
032100970121     C                   MOVEL     'LRN0'        KCOAZ
032200970121     C                   CALL      'BCH10'
032300970121     C                   PARM                    KPJBA
032400911031     C*
032500911105      * STAMPO SEGNALAZIONE CHE CHIUSURA è OK
032600911105      *
032700940210     C                   MOVE      VCDUS         PCUC
032800940210     C                   MOVE      §FLE          PFLE
032900940210     C                   MOVE      §NMI          PNMI
033000110914     C                   MOVE      AMGUD8        PdMI
033100020919     C                   MOVEL(P)  PARAM         KPJBU
033200911105      *
033300911106     C* SOTTOMETTO STAMPA C/A DIRETTI SE CI SONO
033400060127     C     VASMEU        ifGT      *ZEROS
033500950608     C     VMITVA        ORGT      *ZEROS
033600981105     C                   MOVEL     '%0V5'        KCOAZ
033700911104     C                   CALL      'BCH10'
033800911104     C                   PARM                    KPJBA
033900911104     C                   END
034000920311     C* SOTTOMETTO STAMPA SPEDIZIONI CHIUSE CON NOTA ACCREDITO
034100920311     C     *IN80         IFEQ      '1'
034200941014     C                   MOVEL     'LR93'        KCOAZ
034300120628     c                   eval      KJOST = 'H'
034400920311     C                   CALL      'BCH10'
034500920311     C                   PARM                    KPJBA
034600920311     C                   END
034700911106     C                   END
034800910521     C     FINE          TAG
034900921204     C*-                                                   *
035000930426      * Aggiorno su disco
035100930426?    C                   COMMIT
035200930520     C                   SETON                                        LR
035300991019      *-----------------------------------------------------*
035400011113      * Stampa chiusura operatore                           *
035500991019      *-----------------------------------------------------*
035600011113     C     STAMPA        BEGSR
035700011113     c*
035800011114     c                   open      cnc0w4p
035900011113     C                   Z-ADD     *ZEROS        VCONEU
036000011113     C                   Z-ADD     *ZEROS        VASNEU
036100060209     C                   Z-ADD     *ZEROS        Vpos
036200011113     C                   Z-ADD     *ZEROS        VASMCE
036300011113     C                   Z-ADD     *ZEROS        VNRMEU
036400120125     C                   Z-ADD     *ZEROS        VNaMEU
036500011113     C                   Z-ADD     *ZEROS        VMIVAC
036600011113     C                   Z-ADD     *ZEROS        VNRMIV
036700110914     C                   Z-ADD     *ZEROS        VNaMIV
036800011115     C                   Z-ADD     *ZEROS        VINTEU
036900011115     C                   Z-ADD     *ZEROS        VABB
037000011113     C*
037100011113     C                   MOVE      '1'           *INKJ
037200011114     C                   write     TESTAT
037300011113     C                   EXSR      CALC
037400120314     c                   exsr      srtotali
037500011114     C                   write     TRATTO
037600011114     C                   write     TOTALI
037700011113     C                   MOVE      '0'           *INKJ
037800011114     c                   close     cnc0w4p
037900011113     C*
038000011113     c                   endsr
038100911030      *-----------------------------------------------------***********
038200120314      * CALCOLO gli assegni multipli
038300911030      *-----------------------------------------------------***********
038400120314     C     srtotali      BEGSR
038500120314     c* conto assegni multipli
038600120314     C/EXEC SQL
038700120314     C+ DECLARE a2 CURSOR FOR
038800120314     C+ SELECT csmdiv, csmnra, csmdte, csmabi, csmcab from
038900120314     C+ cnass00F join fnarb00f on
039000120314     C+ assaas = arbaas and arblnp=asslnp and
039100120314     C+ arbnrs=assnrs and arbnsp=assnsp join tncsm00f on
039200120314     C+ assnra = csmkey
039300120314     C+ WHERE asstpi = 'M' and assfle = :§§fgs and arbnmi = :§§nmi
039400120314     C+ and arbcuc = :vcdus and arbipp= :§§fgs
039500120314     c+ order by csmkey, csmnra, csmdte, csmabi, csmcab
039600120314     C/END-EXEC
039700120314     c*
039800120314     C/EXEC SQL
039900120314     C+ OPEN a2
040000120314     C/END-EXEC
040100120314     C                   do        *hival
040200120314
040300120314     C/EXEC SQL
040400120314     C+ FETCH NEXT FROM a2 INTO
040500120314     C+ :§vca, :§nra, :§dte, :§abi, :§cab, :§key
040600120314     C/END-EXEC
040700120314
040800120314     C                   select
040900120314     C                   when      sqlcod = 100
041000120314     c                   leave
041100120314     C                   WHEN      SqlCod < 0
041200120314     c                   if        sqlcod <> -913
041300120314     C                   seton                                        H1
041400120314     c                   end
041500120314     c                   seton                                        lr
041600120314     c                   return
041700120314     C                   other
041800120314     c* l'assegno una volta sola
041900120314     c                   if        rifass = savrif
042000120314     c                   iter
042100120314     c                   else
042200120314     c                   eval      savrif = rifass
042300120314     c                   end
042400120314     c*
042500120314     c                   if        §vca = 'EUR'
042600120314     c                   add       1             vnameu
042700120314     c                   else
042800120314     c                   add       1             vnamiv
042900120314     c                   end
043000120314     C                   endsl
043100120314     C                   enddo
043200120314     C/EXEC SQL
043300120314     C+ CLOSE a2
043400120314     C/END-EXEC
043500120314     c                   endsr
043600120314      *-----------------------------------------------------***********
043700120314      * CALCOLO GLI IMPORTI DALLE BOLLE                     *  CALC   *
043800120314      *-----------------------------------------------------***********
043900120314     C     CALC          BEGSR
044000991025     C                   MOVE      *ZERO         §PASEU
044100060213     C                   MOVE      *ZERO         §PASEUp
044200991025     C                   MOVE      *ZERO         §ASNEU
044300991020     C                   MOVE      *ZERO         CTBCAE
044400060213     C                   MOVE      *ZERO         CTBCAEp
044500911030      *
044600020919     C     KNMI          SETLL     FNARB71L                               23
044700911030     C     *IN21         DOUEQ     '1'
044800941018      * AZZERO TUTTI I CAMPI CON DATI INCASSO
044900941018     C                   EXSR      AZZBOL
045000020919     C     KNMI          READE(N)  FNARB71L                               21
045100911030     C     *IN21         IFEQ      '0'
045200941018      *
045300941014      * ESAMINO LA BOLLA LETTA PER VEDERE SE HA INCASSI
045400941014     C                   CLEAR                   DS3A
045500941014     C                   Z-ADD     1             X
045600941014     C     ARBCBO        LOOKUP    T3A(X)                                 26
045700941014     C   26              MOVEL     D3A(X)        DS3A
045800941014     C                   MOVEL     §3ATB1        PORTO             1             PORTO DEL
045900941014      * ASSEGNATO : PRENDO DATI TASSAZIONE 1° BOLLA
046000941014     C     PORTO         IFEQ      'A'
046100991011     C                   MOVEL     '1'           KTRC
046200991011     C     KAR6          CHAIN     FIAR601L                           22
046300941014     C                   END
046400941014      * TASSAZ. 2° BOLLA
046500941014     C     §3ATB2        IFNE      *BLANK
046600991011     C                   MOVEL     '2'           KTRC
046700991011     C     KAR6          CHAIN     FIAR601L                           22
046800941014     C                   END
046900941014      * BOLLA CON CONTRASSEGNO
047000941014     C     §3AFCA        IFNE      0
047100051114     C     KARB          CHAIN     FIAR901L                           22
047200941014     C                   ENDIF
047300911030     C* HO INCASSATO  FATTURA
047400911030     C* ---------------------
047500911030     C     ARBICA        IFEQ      'S'
047600941017     C     ARBICA        OREQ      'I'
047700920325     C     ARBICA        OREQ      'Y'
047800920311     C* SE ESISTONO SPEDIZIONI CHIUSE CON RICH. NOTA ACCR.
047900920311     C* LO SEGNALO
048000920311     C  N80ARBFIP        IFEQ      'C'
048100930526     C     ARBFNC        ANDNE     'E'
048200920311     C                   SETON                                        80
048300920311     C                   END
048400920325     C                   END
048500920325     C*
048600920325     C     ARBICA        IFEQ      'S'
048700941014     C     ARBICA        OREQ      'I'                                           RITORNO I
048800920311     C*
048900991011     C* PRENDO DATA/NR. FATTURA
049000991011     C     PORTO         IFEQ      'A'
049100991011     C     §3ATB2        ORNE      *BLANK
049200991011     C     AR6DFT        IFNE      *ZEROS
049300991011     C                   Z-ADD     AR6NFT        §SNFT
049400991011     C                   Z-ADD     AR6DFT        §SDFT
049500991011     C     AR6KSC        IFNE      0
049600991011     C                   Z-ADD     AR6KSC        §SKSC
049700991011     C                   ELSE
049800941018     C                   Z-ADD     ARBKSC        §SKSC
049900941017     C                   END
050000991011     C                   END
050100991011     C* VERIFICO SE EFFETTUATO INCASSO PARZIALE
050200991011     C                   MOVEL     'A'           KTIP
050300991011     C                   Z-ADD     0             ARIIMP
050400991027     C                   MOVEL     'N'           WTRARI            1
050500070604     C     KARI          CHAIN(n)  FIARI01L                           22
050600991011     C* LOOP FINO A CHE NON TROVO ARIIMP
050700991011     C     *IN22         DOWEQ     '0'
050800991011     C     ARIIMP        ANDEQ     *ZEROS                                        ARBIFP->
050900991027     C                   MOVEL     'S'           WTRARI
051000070604     C     KARI          READE(n)  FIARI01L                               22
051100991011     C                   END
051200991011     C*
051300991011     C     ARIIMP        IFGT      *ZEROS                                        ARBIFP->
051400991027     C* SE ESISTE FIARI IL TIPO INCASSO PARZIALE E' IMPOSTATO E
051500991027     C* NON ESISTE PROGGRESSIVO INCASSATO > 0 IMPOSTO 0 NEL TOTALE
051600991027     C* FATTURA
051700991027     C     ARBFIP        OREQ      'A'
051800991027     C     WTRARI        ANDEQ     'S'
051900991027     C     ARBFIP        OREQ      'C'
052000991027     C     WTRARI        ANDEQ     'S'
052100991018     C                   Z-ADD     ARIIMP        §IFT             12 2
052200991018     C                   MOVEL     ARIDIV        §DIV              3
052300941014     C                   ELSE                                                    X-ARBIFP
052400991011     C                   Z-ADD     AR6IFT        §IFT
052500991013     C                   MOVEL     AR6DIV        §DIV
052600941014     C                   END
052700911030     C* GUARDO IL TIPO INCASSO
052800060210     c                   setoff                                       22
052900941014     C     ARBMIF        IFNE      *BLANK                                        ARBMIF->
053000930409     C                   CLEAR                   DS1A
053100930409     C                   Z-ADD     1             X
053200930409     C     ARBMIF        LOOKUP    T1A(X)                                 22
053300930409     C   22              MOVEL     D1A(X)        DS1A
053400060210     c                   end
053500060210     c* forzo contante
053600060210     c                   if        not *in22
053700060210     c                   eval      §1afca = 'S'
053800060210     c                   end
053900060210     c* se pos non va sommato nel campo §paseu perchè non deve andare negli
054000060210     c* incassi d'attribuire
054100060210     c                   select
054200060210     C* CONTANTI
054300060210     C                   when      §1AFCA = 'S'                                 CONTANTE
054400060210     C                   ADD       §ift          VCONEU
054500060210     C                   ADD       §IFT          §PASEU
054600060210     c* pos
054700060210     C                   when      §1AFCA = 'P'                                 ASSEGNO
054800060210     C                   ADD       §ift          Vpos
054900060213     C                   ADD       §ift          §paseup
055000060330     c* scrive file FIDPI00F in qtemp
055100060330     c                   eval      comaasdpi = arbaas
055200060330     c                   eval      comlnpdpi = arblnp
055300060330     c                   eval      comnrsdpi = arbnrs
055400060330     c                   eval      comnspdpi = arbnsp
055500060330     c                   eval      comdivdpi = §div
055600060330     c                   eval      comvaldpi = §ift
055700060330     c                   exsr      srwrtdpi
055800060210     C* assegni
055900060210     C                   when      §1AFCA = 'B' or §1AFCA = 'C'                 ASSEGNO
056000060210     C                   ADD       §ift          VASNEU
056100060210     C                   ADD       §IFT          §PASEU
056200060210     C                   ENDsl
056300011115     c                   movel     §1afca        fatfca
056400911030     C                   END
056500991011     C                   END
056600920624     C*
056700920624     C   KJ§IFT          IFGT      0
056800920624     C                   SETON                                        77
056900920624     C                   END
057000911104     C*
057100911104     C* HO INCASSATO IL C/ASSEGNO
057200911104     C* -------------------------
057300911030     C     ARBICC        IFEQ      'S'
057400941017     C     AR9CAS        ANDGT     0
057500941014     C     ARBICC        OREQ      'I'
057600941017     C     AR9CAS        ANDGT     0
057700991020     C* GUARDO IL TIPO INCASSO
057800991020     C                   CLEAR                   DS1A
057900060127     C     AR9TIC        IFNE      *BLANKS
058000991020     C                   Z-ADD     1             X
058100991020     C     AR9TIC        LOOKUP    T1A(X)                                 22
058200991020     C   22              MOVEL     D1A(X)        DS1A
058300991020     C                   ELSE
058400991020     C* SE BLK è CONTANTE
058500991020     C                   MOVEL     'S'           §1AFCA
058600991020     C                   END
058700011115     c                   movel     §1afca        casfca
058800991020     C* SE C/ASSEGNO NO MITTENTE CALCOLO IMPORTO INCASSO X CTB
058900060210     C                   IF        §1AFCA='S' or
059000060210     C                             ((§1AFCA='B' or §1afca='C') and
059100060210     C                             §1aFMB<>'M')
059200991118     C                   ADD       AR9CAS        CTBCAE           18 2            C/A NOST
059300011113     C* MEMORIZZO IMPORTO ORIGINALE CONTRASSEGNO PER STAMPA
059400011113     C                   IF        *INKJ
059500011113     c                   z-add     ar9cas        wcimpc
059600011113     c                   movel(p)  ar9vca        wcdivc
059700011113     c                   END
059800011115     C                   END
059900060213     C* SE C/ASSEGNO POS IMPORTO INCASSO X CTB
060000060213     C                   IF        §1AFCA='P'
060100060213     C                   ADD       AR9CAS        CTBCAEp          18 2            C/A NOST
060200060213     C                   END
060300911105     C*
060400060210     c                   select
060500060210     C                   when      §1afca = 'S'                                 CONTANTE
060600991025     C                   ADD       AR9CAS        VCONEU
060700991025     C                   ADD       AR9CAS        §ASNEU
060800991014     C                   Z-ADD     AR9CAS        §ASS
060900060210     C                   when      §1afca = 'P'                                 CONTANTE
061000060210     C                   ADD       AR9CAS        Vpos
061100060210     C                   ADD       AR9CAS        §ASNEU
061200060210     C                   Z-ADD     AR9CAS        §ASS
061300060330     c* scrive file FIDPI00F in qtemp
061400060330     c                   eval      comvaldpi = ar9cas
061500060330     c                   eval      comdivdpi = ar9vca
061600060330     c                   eval      comaasdpi = arbaas
061700060330     c                   eval      comlnpdpi = arblnp
061800060330     c                   eval      comnrsdpi = arbnrs
061900060330     c                   eval      comnspdpi = arbnsp
062000060330     c                   exsr      srwrtdpi
062100060210     C                   when      §1afca = 'B' or §1afca = 'C'                 ASSEGNO
062200060210     C* ASSEGNO MITTENTE
062300911030     C     §1AFMB        IFEQ      'M'
062400941014     C     AR9CAS        IFGT      0
062500060127     C     AR9VCA        IFEQ      §gedmc
062600991014     C                   ADD       1             VNRMEU                         NR.ASSEGNI
062700991018     C                   ADD       AR9CAS        VASMCE                         ASS. MITTE
062800991014     C                   ELSE
062900941017     C                   ADD       1             VNRMIV                         NR.MITT.VA
063000941017     C                   ADD       AR9CAS        VMIVAC                         ASS. MIT.V
063100991014     C                   END
063200060127     C                   END
063300941014     C                   Z-ADD     AR9CAS        §ASS
063400911030     C                   ELSE
063500060210     C* ASSEGNO BARTOLINI
063600991018     C                   ADD       AR9CAS        VASNEU
063700991014     C                   ADD       AR9CAS        §ASNEU                         C/A NOSTRI
063800941014     C                   Z-ADD     AR9CAS        §ASS
063900911030     C                   END
064000060210     C                   ENDSL
064100911030     C                   END
064200920624     C   KJ§ASS          IFGT      0
064300920624     C                   SETON                                        77
064400920624     C                   END
064500920901     C                   END
064600920624     C*
064700920624      *
064800920624     C   KJ
064900920624     CAN 77              DO
065000011113     C* SCRIVO UNA RIGA X LA FATTURA E UNA PER IL CONTRASSEGNO
065100011113     C                   IF        §IFT > 0 AND §ASS > 0
065200011114     C                   Z-ADD     2             cc                2 0
065300011113     C                   ELSE
065400011114     C                   Z-ADD     1             cc
065500011113     C                   END
065600011114     C     1             DO        cc            bb                2 0
065700011113     C* IMPOSTA I CAMPI DELLA STAMPA
065800011113     c                   EXSR      IMPFLD
065900011114     C   07              write     TESTAT
066000011113     c                   setoff                                       07
066100120118     c     '  '          scan      wcnra                                  89
066200120118     c                   if        *in89
066300130402     c                   if        wctpi = 'M'
066400130402     c                   if        wcdiva = 'EUR'
066500120125     c                   add       1             vnameu
066600130402     c                   else
066700130402     c                   add       1             vnamiv
066800120125     c                   end
066900130402     c                   end
067000120118     C                   write     DETTAG
067100110914     c                   else
067200120118     c                   exsr      srcss
067300110914     c                   end
067400011113     C                   ENDDO
067500011113     C*
067600920623     C                   SETOFF                                       77
067700011113     C                   ENDDO
067800920624      *
067900060131     C                   ENDdo
068000060131     c* calcola incassi prepagati da chiusura distinta
068100060131     c                   exsr      srprepag
068200930512     C* RILASCIO RECORD  LETTI
068300070604     C*                  COMMIT
068400911030     C                   ENDSR
068500060328      *-----------------------------------------------------*
068600060330      * scrive file FIDPI00f per incassi pos da usare nella window di scelta
068700060328      *-----------------------------------------------------*
068800060330     C     srwrtdpi      BEGSR
068900060330     c     kdpi          chain     fidpi01l
069000060330     c                   eval      dpiknraz = knraz
069100060330     c                   eval      dpifle = §fle
069200060330     c                   eval      dpiaas = comaasdpi
069300060330     c                   eval      dpilnp = comlnpdpi
069400060330     c                   eval      dpinrs = comnrsdpi
069500060330     c                   eval      dpinsp = comnspdpi
069600060330     c                   eval      dpidiv = comdivdpi
069700061023     c                   if        not %found(fidpi01l)
069800060330     c                   eval      dpivalore = comvaldpi
069900060330     c                   write     fidpi000
070000061023     c                   else
070100061023     c                   add       comvaldpi     dpivalore
070200061023     c                   update    fidpi000
070300060330     c                   end
070400060330     C                   ENDSR
070500060330      *-----------------------------------------------------*
070600060330      * calcola incassi prepagati da chiusura distinta      *
070700060330      *-----------------------------------------------------*
070800060330     C     srprepag      BEGSR
070900060131     c                   clear                   §preeu
071000060213     c                   clear                   §preeup
071100060131     c     kinc          setll     fiinc02l
071200060131     c                   do        *hival
071300070604     c     kinc          reade(n)  fiinc02l
071400060131     c                   if        %eof(fiinc02l)
071500060131     c                   leave
071600060131     c                   end
071700060131     c* solo tipo documento prepagati, solo da chiusura distinta
071800060131     c                   if        inctpd <>'P' or incchd <>'S'
071900060131     c                   iter
072000060131     c                   end
072100060131     c* tipo incasso
072200060131     c                   setoff                                       22
072300060131     c                   if        inctii <> ' '
072400060131     c                   CLEAR                   DS1A
072500060131     C                   Z-ADD     1             X
072600060210     C     inctii        LOOKUP    T1A(X)                                 22
072700060131     C   22              MOVEL     D1A(X)        DS1A
072800060131     c                   end
072900060131     c* forzo contante
073000060131     c                   if        not *in22
073100060131     c                   eval      §1afca = 'S'
073200060131     c                   end
073300060209     c                   select
073400060131     C* CONTANTI
073500060209     C                   when      §1AFCA = 'S'                                 CONTANTE
073600060131     C                   ADD       incimp        VCONEU
073700060210     C                   ADD       incimp        §preEU
073800060209     c* pos
073900060209     C                   when      §1AFCA = 'P'                                 ASSEGNO
074000060209     C                   ADD       incimp        Vpos
074100060213     C                   ADD       incimp        §preeup
074200060330     c* scrive file FIDPI00F in qtemp
074300060330     c                   eval      comvaldpi = incimp
074400060330     c                   eval      comdivdpi = incdiv
074500060330     c                   eval      comaasdpi = incaas
074600060330     c                   eval      comlnpdpi = inclnp
074700060330     c                   eval      comnrsdpi = incnrs
074800060330     c                   eval      comnspdpi = incnsp
074900060330     c                   exsr      srwrtdpi
075000060209     C* assegni
075100060210     C                   when      §1AFCA = 'B' or §1AFCA = 'C'                 ASSEGNO
075200060131     C                   ADD       incimp        VASNEU
075300060210     C                   ADD       incimp        §preEU
075400060209     C                   ENDsl
075500060131     c                   movel     §1afca        prefca
075600060210     C   KJ              IF        (§preeu + vpos) > 0
075700060131     C                   SETON                                        77
075800060131     C                   END
075900060131     c* stampo riga
076000060131     C                   if        *inkj and *in77
076100060131     C* IMPOSTA I CAMPI DELLA STAMPA
076200060131     c                   EXSR      IMPFLDpre
076300060131     C   07              write     TESTAT
076400060131     c                   setoff                                       07
076500060131     C                   write     DETTAG
076600060131     C                   SETOFF                                       77
076700060131     C                   END
076800060131     c                   enddo
076900060131     C                   ENDSR
077000060131      *-----------------------------------------------------*
077100060131      * IMPOSTO I CAMPI IN STAMPA                           *
077200060131      *-----------------------------------------------------*
077300110914     C     srcss         BEGSR
077400120120     c                   z-add     0             conta             2 0
077500120314     c                   eval      csmkey = wcnra
077600120120     c                   eval      wctic = 'C'
077700120314     c     csmkey        setll     tncsm03l
077800111111     c                   do        *hival
077900120314     c     csmkey        reade     tncsm03l
078000120118     c                   if        %eof(tncsm03l)
078100111111     c                   leave
078200111111     c                   end
078300111111     C   07              write     TESTAT
078400111111     c                   setoff                                       07
078500111111     c* più assegni per una spedizione
078600120120     c                   if        conta = 0
078700120120     c                   eval      wctic  = csmtpa + csmtpi
078800120120     c                   eval      wcnra  = csmnra
078900120120     c                   movel     csmabi        wcabi
079000120120     c                   movel     csmcab        wccab
079100120120     c                   eval      wcdiva  = csmdiv
079200120120     c                   eval      wcimpi  = csmidc
079300120120     c                   movel     csmdte        datiso
079400120120     c                   movel     datiso        dateur
079500120120     c                   movel     dateur        wcdta
079600120120     c     wcdta         comp      0                                      27
079700120120     C                   write     dettag
079800120120     c                   z-add     1             conta
079900120120     c                   else
080000120120     c                   eval      wctica = csmtpa + csmtpi
080100120120     c                   eval      wcnraa = csmnra
080200120120     c                   movel     csmabi        wcabia
080300120120     c                   movel     csmcab        wccaba
080400120120     c                   eval      wcdivaa = csmdiv
080500120120     c                   eval      wcimpia = csmidc
080600120120     c                   movel     csmdte        datiso
080700120120     c                   movel     datiso        dateur
080800120120     c                   movel     dateur        wcdtaa
080900120120     c     wcdtaa        comp      0                                      27
081000110914     C                   write     detass
081100120120     c                   end
081200110914     c                   enddo
081300110914     C                   ENDSR
081400110914      *-----------------------------------------------------*
081500110914      * IMPOSTO I CAMPI IN STAMPA                           *
081600110914      *-----------------------------------------------------*
081700110914     C     IMPFLD        BEGSR
081800011114     C*
081900011114     C                   Z-ADD     0             WCLNP
082000011114     C                   Z-ADD     0             WCLNA
082100011114     C                   Z-ADD     0             WCNRS
082200011114     C                   Z-ADD     0             WCNSP
082300011114     C                   Z-ADD     0             GG                2 0
082400011114     C                   Z-ADD     0             MM                2 0
082500011114     C                   Z-ADD     0             AA                2 0
082600011114     C                   Z-ADD     0             WCKSC
082700011114     C                   MOVEL     *blanks       DES25            25
082800011114     C                   Z-ADD     0             WCNDC
082900011114     C                   Z-ADD     0             WCDDC
083000011114     C                   Z-ADD     0             WCnft
083100011114     C                   Z-ADD     0             WCDFT
083200011114     C* PER PRIMA RIGA IMPOSTO TUTTI I CAMPI
083300011114     C                   IF        bb = 1
083400011114     C                   MOVE      ARBLNA        WCLNA
083500011113     C                   Z-ADD     ARBLNP        WCLNP
083600011113     C                   Z-ADD     ARBNRS        WCNRS
083700011113     C                   Z-ADD     ARBNSP        WCNSP
083800011113     C                   MOVE      ARBMGS        GG                2 0
083900011113     C                   MOVEL     ARBMGS        MM                2 0
084000011113     C                   MOVE      ARBAAS        AA                2 0
084100011113     C                   IF        §SKSC <> 0
084200011113     C                   Z-ADD     §SKSC         WCKSC
084300011113     C                   ELSE
084400011113     C                   Z-ADD     ARBKSC        WCKSC
084500011113     C                   END
084600011113     C                   MOVEL     ARBRSD        DES25            25
084700011113     C                   Z-ADD     ARBNDC        WCNDC
084800011113     C                   MOVE      ARBDDC        DATISO
084900011113     C                   MOVE      datiso        DATeur
085000011113     C                   MOVE      DATeur        WCDDC
085100011113     C* SE HO DUE RIGHE: STAMPA PRIMA CONTRASSEGNO E POI ASSEGNATO
085200011114     C                   IF        cc = 2
085300011113     C                   MOVE      'C'           WCTIP
085400011113     C                   ELSE
085500011113     C                   IF        §IFT > 0
085600011113     C                   MOVE      'A'           WCTIP
085700011113     C                   ELSE
085800011113     C                   MOVE      'C'           WCTIP
085900011113     C                   END
086000011113     C                   END
086100011113     C                   ELSE
086200011113     C                   MOVE      'A'           WCTIP
086300011113     C                   END
086400011114     C* SE RIGA ASSEGNATO VALORIZZO DATI FATTURA
086500011114     C                   IF        WCTIP = 'A'
086600011114     C                   MOVE      §SDFT         G08INV
086700011114     C                   MOVE      *ZEROS        G08DAT
086800011114     C                   MOVEL     '3'           G08ERR
086900011114     C                   CALL      'XSRDA8'
087000011114     C                   PARM                    WLBDA8
087100011114     C                   MOVEL     G08DAT        WCDFT             6 0
087200011114     C                   MOVE      G08DAT        W2                2 0
087300011114     C                   MOVE      W2            WCDFT
087400011114     C                   Z-ADD     §SNFT         WCNFT
087500011114     C                   END
087600011114     c     WCDFT         comp      0                                      18
087700011114     c     GG            comp      0                                      57
087800011114     c     WCDDC         comp      0                                      58
087900011114     C*
088000011113     c                   if        wctip = 'C'
088100011113     C                   MOVEL     AR9VCA        WCDIVI
088200011113     C                   Z-ADD     AR9CAS        WCIMPI
088300011113     C                   movel     AR9tic        WCtic
088400011113     c                   else
088500011113     C                   MOVEL     §div          WCDIVc
088600011113     C                   Z-ADD     §ift          WCIMPc
088700011113     C                   MOVEL     §div          WCDIVI
088800011113     C                   Z-ADD     §ift          WCIMPI
088900011113     C                   movel     ARBmif        WCtic
089000011113     C                   END
089100011115     c* Solo per pagamenti non contanti
089200060210     c                   if        (wctip = 'A' and (fatfca = 'B' or
089300060210     c                             fatfca = 'C')) or
089400060210     c                             (wctip = 'C' and (casfca = 'B' or
089500060331     c                             casfca = 'C'))
089600011113     c                   exsr      srass
089700011116     C                   ELSE
089800011116     C                   MOVEL     *BLANKS       WCNRA
089900011116     C                   Z-ADD     0             WCDTA
090000011116     C                   MOVEL     *BLANKS       WCABI
090100011116     C                   MOVEL     *BLANKS       WCCAB
090200011116     C                   MOVEL     *BLANKS       WCDIVA
090300011116     C                   End
090400011114     c     wcdta         comp      0                                      27
090500011113     c*
090600011113     C                   ENDSR
090700060131      *-----------------------------------------------------*
090800060131      * IMPOSTO I CAMPI IN STAMPA prepagati                 *
090900060131      *-----------------------------------------------------*
091000060131     C     IMPFLDpre     BEGSR
091100060131     C*
091200060131     C                   Z-ADD     0             WCLNP
091300060131     C                   Z-ADD     0             WCLNA
091400060131     C                   Z-ADD     0             WCNRS
091500060131     C                   Z-ADD     0             WCNSP
091600060131     C                   Z-ADD     0             GG                2 0
091700060131     C                   Z-ADD     0             MM                2 0
091800060131     C                   Z-ADD     0             AA                2 0
091900060131     C                   Z-ADD     0             WCKSC
092000060131     C                   MOVEL     *blanks       DES25            25
092100060131     C                   Z-ADD     0             WCNDC
092200060131     C                   Z-ADD     0             WCDDC
092300060131     C                   Z-ADD     0             WCnft
092400060131     C                   Z-ADD     0             WCDFT
092500060131     c     kspear6       chain     fiar601l
092600060131     c                   if        %found(fiar601l)
092700060131     c     kspeblp       chain     fnblp01l
092800060131     c                   if        %found(fnblp01l)
092900060131     C                   MOVE      blplna        WCLNA
093000060131     C                   MOVE      blpMGS        GG                2 0
093100060131     C                   MOVEL     blpMGS        MM                2 0
093200060131     C                   MOVE      blpAAS        AA                2 0
093300060131     C                   IF        ar6KSC <> 0
093400060131     C                   Z-ADD     ar6ksc        WCKSC
093500060131     C                   ELSE
093600060131     C                   Z-ADD     blpKSC        WCKSC
093700060131     C                   END
093800060131     C                   MOVEL     blpRSD        DES25            25
093900060131     c                   end
094000060131     c                   end
094100060131     C                   Z-ADD     incLNP        WCLNP
094200060131     C                   Z-ADD     incNRS        WCNRS
094300060131     C                   Z-ADD     incNSP        WCNSP
094400060131     C                   MOVE      'P'           WCTIP
094500060131     C                   MOVE      incdft        G08INV
094600060131     C                   MOVE      *ZEROS        G08DAT
094700060131     C                   MOVEL     '3'           G08ERR
094800060131     C                   CALL      'XSRDA8'
094900060131     C                   PARM                    WLBDA8
095000060131     C                   MOVEL     G08DAT        WCDFT             6 0
095100060131     C                   MOVE      G08DAT        W2                2 0
095200060131     C                   MOVE      W2            WCDFT
095300060131     C                   Z-ADD     incnft        WCNFT
095400060131     c     WCDFT         comp      0                                      18
095500060131     c     GG            comp      0                                      57
095600060131     c     WCDDC         comp      0                                      58
095700060131     C* non sono previsti gli incassi parziali
095800060131     C                   MOVEL     incdiv        WCDIVc
095900060131     C                   Z-ADD     incimp        WCIMPc
096000060131     C                   MOVEL     incdiv        WCDIVI
096100060131     C                   Z-ADD     incimp        WCIMPI
096200060131     C                   movel     inctii        WCtic
096300060209     c* Solo per pagamenti con assegno
096400060210     c                   if        prefca  = 'B' or prefca = 'C'
096500060404     c                   exsr      srasspre
096600060131     C                   ELSE
096700060131     C                   MOVEL     *BLANKS       WCNRA
096800060131     C                   Z-ADD     0             WCDTA
096900060131     C                   MOVEL     *BLANKS       WCABI
097000060131     C                   MOVEL     *BLANKS       WCCAB
097100060131     C                   MOVEL     *BLANKS       WCDIVA
097200060131     C                   End
097300060131     c     wcdta         comp      0                                      27
097400060131     c*
097500060131     C                   ENDSR
097600011115      *-----------------------------------------------------*
097700011115      * AGGANCIO L'ARCHIVIO DEGLI ASSEGNI                   *
097800011115      *-----------------------------------------------------*
097900060404     C     SRASSpre      BEGSR
098000011115     C*
098100060404     C     Kspeass       SETLL     CNASS01L
098200011113     C                   DO        *HIVAL
098300060404     C     Kspeass       READE     CNASS01L                               06
098400011113     C   06              LEAVE
098500011113     C                   IF        ASSTPD = 'T' OR ASSTPD = WCTIP
098600011113     C                   MOVEL     ASSNRA        WCNRA
098700011114     C                   MOVE      ASSDTE        DatISO
098800011114     C                   MOVE      DATISO        DatEUR
098900011113     C                   MOVE      DATEUR        WCDTA
099000011113     C                   MOVEL     ASSABI        WCABI
099100011113     C                   MOVEL     ASSCAB        WCCAB
099200011114     C                   MOVEL     ASSvca        WCDIVA
099300011113     C                   LEAVE
099400011113     C                   END
099500011113     C                   ENDDO
099600011113     C                   ENDSR
099700060404      *-----------------------------------------------------*
099800060404      * AGGANCIO L'ARCHIVIO DEGLI ASSEGNI                   *
099900060404      *-----------------------------------------------------*
100000060404     C     SRASS         BEGSR
100100060404     C*
100200060404     C     KAAS          SETLL     CNASS01L
100300060404     C                   DO        *HIVAL
100400060404     C     KAAS          READE     CNASS01L                               06
100500060404     C   06              LEAVE
100600060404     C                   IF        ASSTPD = 'T' OR ASSTPD = WCTIP
100700060404     C                   MOVEL     ASSNRA        WCNRA
100800120125     C                   MOVEL     ASStpi        WCtpi             1
100900111020     c                   if        assdte = 0
101000111020     c                   clear                   wcdta
101100111020     c                   else
101200060404     C                   MOVE      ASSDTE        DatISO
101300060404     C                   MOVE      DATISO        DatEUR
101400060404     C                   MOVE      DATEUR        WCDTA
101500111020     c                   end
101600060404     C                   MOVEL     ASSABI        WCABI
101700060404     C                   MOVEL     ASSCAB        WCCAB
101800060404     C                   MOVEL     ASSvca        WCDIVA
101900060404     C                   LEAVE
102000060404     C                   END
102100060404     C                   ENDDO
102200060404     C                   ENDSR
102300941018      *-----------------------------------------------------***********
102400941018      * AZZERO TUTTI I CAMPI CON DATI INCASSO LETTI         AZZBOL    *
102500941018      *-----------------------------------------------------***********
102600941018     C     AZZBOL        BEGSR
102700991013     C                   MOVEL     *BLANK        §DIV
102800991013     C                   Z-ADD     *ZEROS        §SNFT
102900991013     C                   Z-ADD     *ZEROS        §SDFT
103000991013     C                   Z-ADD     *ZEROS        §SKSC
103100991013     C                   Z-ADD     *ZERO         §IFT
103200991013     C                   Z-ADD     *ZEROS        §ASS
103300941018     C                   MOVE      *ZEROS        AR6NFT
103400941018     C                   MOVE      *ZEROS        AR6DFT
103500941018     C                   MOVE      *ZEROS        AR6IFT
103600991011     C                   MOVE      *ZEROS        AR6KSC
103700941018     C                   MOVE      *ZEROS        AR9CAS
103800941018     C                   MOVE      *BLANK        AR9VCA
103900941018     C                   MOVE      *BLANK        AR9TIC
104000011116     C                   MOVE      *BLANK        WCDIVC
104100011115     C                   z-add     0             wcimpc
104200011115     C                   MOVEl     *blanks       fatfca
104300011115     C                   MOVEl     *blanks       casfca
104400120125     C                   MOVEL     *BLANKS       WCtpi
104500120125     C                   MOVEL     *BLANKS       WCNRA
104600011115     C                   Z-ADD     0             WCDTA
104700011115     C                   MOVEL     *BLANKS       WCABI
104800011115     C                   MOVEL     *BLANKS       WCCAB
104900011115     C                   MOVEL     *BLANKS       WCDIVA
105000941018     C                   ENDSR
105100900524      *
105200900515      *-----------------------------------------------------***********
105300900515      * CONTROLLI FORMATO 1                                 *  CTR1   *
105400900515      *-----------------------------------------------------***********
105500900515     C     CTR1          BEGSR
105600060328     c*
105700060328     c* verifico se scelte finestra incassi
105800060328     c                   exsr      srcalcola
105900900515      *
106000911030     C                   SETOFF                                       19
106100060424     C* contante
106200060424     c     vconeu        comp      vconeu1                            2828
106300060424     C* ns assegni
106400060424     c     vasneu        comp      vasneu1                            2929
106500060424     C* pos
106600060424     c     vpos          comp      vpos1                              3131
106700911030     C* CONTROLLO QUADRATURE
106800060209     C                   eval      vinteu = VASNEU +vconeu +vpos
106900060130     C* SOTTRAGGO IL TOTALE INCASSATO AL TOTALE DA INTROITARE
107000060130     C* E CALCOLO COSI' L'ABBUONO
107100060130     C     VINTeu        SUB       VINCeu        VABB
107200911030     C* LO RENDO POSITIVO
107300911030     C     VABB          IFLT      *ZEROS
107400911030     C     VABB          MULT      -1            §ABB
107500911030     C                   ELSE
107600911030     C                   Z-ADD     VABB          §ABB
107700911030     C                   END
107800911030     C* CONTROLLO IL LIMITE SUPERIORE DELL'ABBUOBO OPERATORE
107900991011     C                   MOVEL     'GEI'         TBECOD
108000060127     C                   MOVEL     §GEDmc        TBEKE1
108100991011     C     KTBE          CHAIN     TNTBE01L                           22
108200991011     C  N22              MOVEL     TBEUNI        DGEI
108300991011     C     §ABB          COMP      §GEMAO                             30
108400911030     C   30              SETON                                        19
108500060127     C* SE TOTALE INCASSATO NON COPRE IL TOTALE DEGLI
108600991018     C* ASSEGNI INTESTATO A BARTOLINI DO ERRORE
108700991108     C     VASNEU        IFNE      0
108800060127     C     VASNEU        SUB       §abb          DAINCE           15 3
108900991108     C     VINCEU        IFLT      DAINCE
109000991018     C                   SETON                                        2919
109100991018     C                   END
109200991108     C                   END
109300120123     c                   if        vasmeu <> svasmeu or
109400120123     c                             vasmce <> svasmce or
109500120123     c                             vasmeu = 0
109600991014     C     VASMEU        COMP      VASMCE                             3434
109700991014     C   34              SETON                                        19
109800120123     c                   eval      svasmeu = vasmeu
109900120123     c                   eval      svasmce = vasmce
110000120123     c                   end
110100941017     C* QUADR. C/A MITT. IN VALUTA
110200120123     c                   if        vmitva <> svmitva or
110300120123     c                             vmivac <> svmivac or
110400120123     c                             vmitva = 0
110500941019     C     VMITVA        COMP      VMIVAC                             3333
110600941019     C   33              SETON                                        19
110700120123     c                   eval      svmitva = vmitva
110800120123     c                   eval      svmivac = vmivac
110900120123     c                   end
111000911030     C* NON HO DISTINTE CHIUSE DA CONTABILIZZARE
111100060127     C     VINTEU        ifeq      *ZEROS
111200991014     C     VASMCE        ANDEQ     *ZEROS
111300911213     C     *IN23         ANDEQ     '0'
111400911030     C                   SETON                                        3219
111500911030     C                   END
111600900515     C     FICTR1        ENDSR
111700911031      *-----------------------------------------------------***********
111800060328      * finestre incassi
111900911031      *-----------------------------------------------------***********
112000060328     C     srcalcola     BEGSR
112100060328     c* contante
112200060328     c                   if        selconv <> ' '
112300060328     c                   if        vconeu <> 0
112400060328     c                   exsr      srwincon
112500060328     c                   end
112600060328     c                   clear                   selconv
112700060328     c                   end
112800060328     c* assegni bartolini
112900060328     c                   if        selasbv <> ' '
113000060328     c                   if        vasneu <> 0
113100060328     c                   clear                   cnc0wb00s
113200060330     c                   eval      $DIVISA ='EUR'
113300060328     c                   exsr      srwinass
113400060328     c                   end
113500060328     c                   clear                   selasbv
113600060328     c                   end
113700060328     c* pos
113800060328     c                   if        selposv <> ' '
113900060330     c                   if        vpos <> 0
114000060330     c                   exsr      srwinpos
114100060330     c                   end
114200060328     c                   clear                   selposv
114300060328     c                   end
114400060328     c* assegni mittente eur
114500060328     c                   if        selamv <> ' '
114600060328     c                   if        vasmce <> 0
114700060328     c                   clear                   cnc0wb00s
114800060328     c                   eval      $DIVISA ='EUR'
114900060328     c                   eval      $tpiass= 'M'
115000060328     c                   exsr      srwinass
115100060328     c                   end
115200060328     c                   clear                   selamv
115300060328     c                   end
115400060328     c* assegni mittente divisa
115500060328     c                   if        selamvv <> ' '
115600060328     c                   if        vmivac <> 0
115700060328     c                   clear                   cnc0wb00s
115800060328     c                   eval      $tpiass= 'M'
115900060328     c                   exsr      srwinass
116000060328     c                   end
116100060328     c                   clear                   selamvv
116200060328     c                   end
116300060329     C*
116400060328     C                   ENDSR
116500060328      *-----------------------------------------------------***********
116600060328      * window contante
116700060328      *-----------------------------------------------------***********
116800060328     C     srwincon      BEGSR
116900100514      * richiamo finestra per inserimento totale dei mandatini versati in cassaforte
117000100521     C                   eval      ptotcas  = vconeu
117100100514     c                   call      'CNC0WA02R'
117200100514     C                   parm                    ptotcas
117300100514      *
117400060330     c                   clear                   cnc0wa00s
117500060330     c                   eval      £DIVISA ='EUR'
117600100514     c                   eval      £IMPTOTALE = vconeu - ptotcas
117700060330     c                   eval      kpjbu = cnc0wa00s
117800060330     c                   call      'CNC0WA00R'
117900060328     c                   parm                    kpjba
118000060330     c                   eval      cnc0wa00s = kpjbu
118100100604     c*mm                if        £RTNCMD = F6 and
118200100604     c*mm                          £RTNCODE ='DONE'
118300100525     c                   add       ptotcas       £IMPIMMESSO
118400060331     c                   sub       vconeuh       vinceu
118500060331     c                   add       £IMPIMMESSO   vinceu
118600060424     c                   sub       vconeuh       vconeu1
118700060424     c                   add       £IMPIMMESSO   vconeu1
118800060331     c                   z-add     £IMPIMMESSO   vconeuh
118900100604     c*mm                end
119000060328     C                   ENDSR
119100060330      *-----------------------------------------------------***********
119200060330      * window pos
119300060330      *-----------------------------------------------------***********
119400060330     C     srwinpos      BEGSR
119500060330     c                   clear                   cnc0wc00s
119600060330     c                   eval      kpjbu = cnc0wc00s
119700060330     c                   call      'CNC0WC00R'
119800060330     c                   parm                    kpjba
119900060330     c                   eval      cnc0wc00s = kpjbu
120000060330     c                   if        xRTNCMD = F6 and
120100060330     c                             xRTNCODE ='DONE'
120200060331     c                   sub       vposh         vinceu
120300060331     c                   add       xIMPIMMESSO   vinceu
120400060424     c                   sub       vposh         vpos1
120500060424     c                   add       xIMPIMMESSO   vpos1
120600060331     c                   z-add     xIMPIMMESSO   vposh
120700060330     c                   end
120800060330     C                   ENDSR
120900060328      *-----------------------------------------------------***********
121000060328      * window assegni
121100060328      *-----------------------------------------------------***********
121200060328     C     srwinass      BEGSR
121300060328     c                   eval      $fle =§fle
121400060328     c                   eval      kpjbu = cnc0wb00s
121500060427     c                   call      'CNC0WB01R'
121600060328     c                   parm                    kpjba
121700060328     c                   eval      cnc0wb00s = kpjbu
121800060331     c                   if        $RTNCMD = F6 and
121900060331     c                             $RTNCODE ='DONE'
122000060331     c                   if        $tpiass <> 'M'
122100060331     c                   sub       vasneuh       vinceu
122200060331     c                   add       $IMPIMMESSO   vinceu
122300060424     c                   sub       vasneuh       vasneu1
122400060424     c                   add       $IMPIMMESSO   vasneu1
122500060331     c                   z-add     $IMPIMMESSO   vasneuh
122600060331     c                   else
122700060331     c                   if        $divisa = 'EUR'
122800060331     c                   z-add     $IMPIMMESSO   vasmeu
122900120118     c                   z-add     $nraIMMESSO   vnameu
123000060331     c                   else
123100060331     c                   z-add     $IMPIMMESSO   vmitva
123200120118     c                   z-add     $nraIMMESSO   vnamiv
123300060331     c                   end
123400060331     c                   end
123500060331     c                   end
123600060328     C                   ENDSR
123700060328      *-----------------------------------------------------***********
123800060328      * ALLOCAZIONE FILE                                    *  ALCOBJ *
123900060328      *-----------------------------------------------------***********
124000060328     C     ALCOBJ        BEGSR
124100911105     C                   SETOFF                                       91
124200991014     C* DETERMINO SU CHE MEMBRO SCRIVERE DI FNMOV00T E SE NON è ALLOCATO
124300020920     C                   Z-ADD     48            LUNG             15 5
124400020920     C                   Z-ADD     44            LUNCMD           15 5
124500911104     C* ----------------------------------------------------------------
124600911031     C  N98              DO
124700911031     C** FILIALE CHE CHIUDE    PER APERTURA MEMBRO
124800991014     C** CONTROLLO SE ESISTE IL MEMBRO X FNMOV00T
124900911031     C                   SETOFF                                       20
125000020920     C                   MOVEl(p)  CMD3(1)       COMMAN           80
125100020920     c                   eval      %subst(comman:42:6) = §mbr
125200911031     C                   CALL      'QCMDEXC'                            20
125300911031     C                   PARM                    COMMAN
125400911031     C                   PARM                    LUNG
125500911031     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
125600911031     C   20              DO
125700020920     C                   MOVEl(p)  CMD1(1)       COMMAN
125800020920     c                   eval      %subst(comman:42:6) = §mbr
125900911031     C                   CALL      'QCMDEXC'
126000911031     C                   PARM                    COMMAN
126100911031     C                   PARM                    LUNG
126200911031     C                   END
126300911031     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
126400020920     C                   MOVEl(p)  CMD2(1)       COMMAN
126500020920     c                   eval      %subst(comman:42:6) = §mbr
126600911031     C                   CALL      'QCMDEXC'
126700911031     C                   PARM                    COMMAN
126800911031     C                   PARM                    LUNG
126900911031     C* GUARDO SE è ALLOCATO
127000020920     C                   MOVEl(p)  CMA(1)        CMDX             44
127100020920     c                   eval      %subst(cmdx:37:6) = §mbr
127200911031     C                   CALL      'QCMDEXC'                            91
127300911031     C                   PARM                    CMDX
127400911031     C                   PARM                    LUNCMD
127500911105     C   91              SETON                                          1936
127600991014     C  N91              OPEN      FNMOV00T
127700911105     C* SONO RIUSCITA AD ALLOCARLO  :NON LO DEVO PIù FARE
127800911105     C  N91              SETON                                          98
127900911105     C                   END
128000020920     C* MEMBRO = §FLE + 046
128100911031     C  N91              DO
128200911031     C* GUARDO SE IL MEMBRO è VUOTO
128300930426     C                   EXSR      TRUL51
128400991014     C                   MOVEL     'FNMOV00T'    FILE
128500020920     C                   MOVEL(P)  §membro       MBR
128600930426     C                   CALL      'TRUL51C'
128700930426     C                   PARM                    LIBR
128800930426     C                   PARM                    FILE
128900930426     C                   PARM                    MBR
129000911031     C                   PARM                    NRR
129100911031     C     NRR           COMP      *ZEROS                             9191
129200911031     C   91              SETON                                        1937
129300911031     C                   END
129400941014     C* DETERMINO SU CHE MEMBRO SCRIVERE DI FNCAS00T E SE NON è ALLOCATO
129500911104     C* ----------------------------------------------------------------
129600911104     C  N99              DO
129700911104     C** FILIALE CHE CHIUDE    PER APERTURA MEMBRO
129800941014     C** CONTROLLO SE ESISTE IL MEMBRO X FNCAS00T
129900911104     C                   SETOFF                                       20
130000020920     C                   MOVEl(p)  CMD3(2)       COMMAN           80
130100020920     c                   eval      %subst(comman:42:6) = §mbr
130200911104     C                   CALL      'QCMDEXC'                            20
130300911104     C                   PARM                    COMMAN
130400911104     C                   PARM                    LUNG
130500911104     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
130600911104     C   20              DO
130700020920     C                   MOVEl(P)  CMD1(2)       COMMAN
130800020920     c                   eval      %subst(comman:42:6) = §mbr
130900911104     C                   CALL      'QCMDEXC'
131000911104     C                   PARM                    COMMAN
131100911104     C                   PARM                    LUNG
131200911104     C                   END
131300911104     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
131400020920     C                   MOVEl(P)  CMD2(2)       COMMAN
131500020920     c                   eval      %subst(comman:42:6) = §mbr
131600911104     C                   CALL      'QCMDEXC'
131700911104     C                   PARM                    COMMAN
131800911104     C                   PARM                    LUNG
131900911104     C* GUARDO SE è ALLOCATO
132000020920     C                   MOVEl(P)  CMA(2)        CMDX
132100020920     c                   eval      %subst(cmdx:37:6) = §mbr
132200911104     C                   CALL      'QCMDEXC'                            91
132300911104     C                   PARM                    CMDX
132400911104     C                   PARM                    LUNCMD
132500911105     C   91              SETON                                          1938
132600941014     C  N91              OPEN      FNCAS00T
132700911105     C* SONO RIUSCITA AD ALLOCARLO  :NON LO DEVO PIù FARE
132800911105     C  N91              SETON                                          99
132900911105     C                   END
133000911104     C  N91              DO
133100911104     C* GUARDO SE IL MEMBRO è VUOTO
133200930426     C                   EXSR      TRUL51
133300941014     C                   MOVEL     'FNCAS00T'    FILE
133400020920     C                   MOVEL(P)  §membro       MBR
133500930426     C                   CALL      'TRUL51C'
133600930426     C                   PARM                    LIBR
133700930426     C                   PARM                    FILE
133800930426     C                   PARM                    MBR
133900911104     C                   PARM                    NRR
134000911104     C     NRR           COMP      *ZEROS                             9191
134100911104     C   91              SETON                                        1939
134200911104     C                   END
134300971125     C* VARIAZIONI BOLLE IL MEMBRO è SEMPRE SEDE
134400930426      *-----------------------------------------------------------
134500020806     c                   movel     simfel        $simfel
134600020806     C                   MOVE      ')'           VAR7              7
134700020806     c                   movel     $mbr6         var7
134800020806     C                   MOVEL     VAR7          VAR8              8
134900020806     C                   MOVE      ')'           VAR8
135000950125     C* ALLOCO TUTTO FNCAF00T
135100950125     C  N67              DO
135200021014     C* Impostazione DS per chiamare QCAPCMD
135300021014     C                   clear                   qusec
135400021014     C                   eval      qusbprv = %size(qusec)
135500021014     C                   clear                   qcap0100
135600021014     C                   eval      qcabcsdh = '0'
135700021014     C                   eval      qcapa = '0'
135800021014     C                   eval      qcacmdss = '0'
135900021014     C                   eval      qcaerved = x'000000000000000000'
136000021014     C* Impostazione stringa comando.
136100120424     C                   eval      pcmd01 = cma(3)
136200040407     C                   eval      %subst(pcmd01: 37: 3) = $simfel
136300040407     C                   eval      %subst(pcmd01: 40: 3) = $simfel
136400021014     C* Esecuzione comando.
136500021014     C                   call(e)   'QCAPCMD'
136600021014     C                   parm                    pcmd01
136700021014     C                   parm                    pcmd02
136800021014     C                   parm      qcap0100      pcmd03
136900021014     C                   parm                    pcmd04
137000021014     C                   parm                    pcmd05
137100021014     C                   parm                    pcmd06
137200021014     C                   parm                    pcmd07
137300021014     C                   parm                    pcmd08
137400021014     C                   parm                    qusec
137500021014     C* A questo punto QUSEI contiene l'eventuale MSGID di errore:
137600021014     C*  se è vuoto il comando è stato eseguito correttamente.
137700021014     C*  se è CPF1085 il file non ha membri quindi prosegue normalmente
137800021014     C                   if        qusei = *blanks  or
137900021014     C                             qusei = 'CPF1085'
138000021014     c                   seton                                        67
138100021014     C                   end
138200021014     C*  se è CPF1002 il file è già allocato
138300021014     C                   if        qusei = 'CPF1002'
138400021014     c                   seton                                        1938
138500021014     C                   end
138600950125     C                   END
138700911031     C                   ENDSR
138800930426      *----------------------------------------------------------------
138900930426      * PULIZIA PARAMETRI X TRUL51C
139000930426      *----------------------------------------------------------------
139100930426     C     TRUL51        BEGSR
139200930426     C                   MOVEL     KNSIF         LIBR             10
139300930426     C                   MOVEL     *BLANK        FILE             10
139400930426     C                   MOVEL     *BLANK        MBR              10
139500930426     C                   Z-ADD     0             NRR              10 0
139600930426     C                   ENDSR
139700991018      *-----------------------------------------------------*
139800991018      * SCRITTURA FILE MANDATI D'INTROITO                   *
139900991018      *-----------------------------------------------------*
140000991018     C     WRTARM        BEGSR
140100991018     C*
140200060130     C                   MOVE      §FLE          mdiFLE
140300060130     C                   MOVE      VCDUS         mdiCUC
140400060130     C                   MOVE      'O'           mditmi
140500060130     C                   MOVE      AMGUD8        mdiDMI
140600060130     C                   MOVE      §NMI          mdiNMI
140700060130     C                   MOVE      ora           mdiora
140800060403     c* l'abbuono lo sottraggo dal contante
140900060421     C                   eval      mdiicon = £IMPIMMESSO
141000060130     C                   Z-ADD     VasnEU        mdiiass
141100060130     C                   Z-ADD     Vpos          mdiipos
141200060404     C                   MOVEL     'S'           mdiFPC
141300060127     C* imposto a blanks flag versamento
141400060131     C                   MOVEL     *BLANKS       mdivIE
141500060130     C                   WRITE     yfmdi000
141600991018     C*
141700991018     C                   ENDSR
141800911031      *-----------------------------------------------------***********
141900060213      * AGGIORNAMENTO ARCHIVI x contante e assegni (Incassi d'attribuire)
142000911031      *-----------------------------------------------------***********
142100911031     C     AGGIOR        BEGSR
142200060214     C                   MOVE      VCDUS         WCOMO6
142300060213     C                   MOVEL     §FLE          WCOMO6
142400060214     c                   z-add     0             subrig
142500100714     c                   z-add     0             savlna
142600991125     C*
142700020919     C     KNMI          SETLL     FNARB71L
142800911031     C     *IN21         DOUEQ     '1'
142900131212      **
143000131212     c     ritenta       tag
143100131212     C     KNMI          READE(e)  FNARB71L                               21
143200131212      **
143300131212      *  se il record di ARB è allocato da qualcuno
143400131212      **
143500131212     c                   if        %Error
143600131212      * se il record è allocato mando messaggio info all'utente che alloca il rcd
143700131212     c                   clear                   trul82ds
143800131212     c                   eval      ul82§rrn = arb_nrr
143900131212     c                   eval      ul82§fil = 'FNARB00F'
144000131212     c                   eval      ul82§win = 'S'
144100131212     c                   eval      ul82§f7  = 'S'
144200131212     c                   eval      ul82§num = 2
144300131212     c                   eval      ul82§att = 2
144400131212     c                   eval      ul82§mss = 'Si sta bloccando la << Chiusura '
144500131212     c                             + ' Operatore. >> ' +
144600131212     c                             ' SI PREGA DI USCIRE dal lavoro!'
144700131213     c                   Eval      UL82§msw = 'ATTENZIONE La Chiusura Operatore-
144800131213     c                              è BLOCCATA F12 x Riprovare'
144900131212      *
145000131212      * chiamo il pgm che manda il messaggio info all'utente
145100131212     c                   call(e)   'TRUL82R'
145200131212     c                   parm                    trul82ds
145300131213      *  Deve sempre rileggere il record  sia x provare se ancora allocato
145400131213      *  che se liberato dall'altro lavoro andrebbe avanti facendo un aggiornamento
145500131213      *  senza lettura del record.
145600131212     c                   goto      ritenta
145700131212     c                   end
145800131212      **  Trovato
145900131212      *
146000911031     C     *IN21         IFEQ      '0'
146100911031     C* HO INCASSATO  FATTURA
146200911205     C                   MOVE      *ZEROS        §IFT
146300911205     C                   MOVE      *ZEROS        §§IFT
146400060210     c* guardo il tipo incasso perchè devo escludere i pos
146500060210     C     ARBMIF        IFNE      *BLANK                                        ARBMIF->
146600060210     C                   CLEAR                   DS1A
146700060210     C                   Z-ADD     1             X
146800060210     C     ARBMIF        LOOKUP    T1A(X)                                 22
146900060210     c                   if        *in22
147000060210     C                   MOVEL     D1A(X)        DS1A
147100060210     c                   if        §1afca = 'P'
147200060210     c                   iter
147300060210     c                   end
147400060210     c                   end
147500060210     c                   end
147600941014      * ANALIZZO LA BOLLA
147700941014     C                   CLEAR                   DS3A
147800941014     C                   Z-ADD     1             X
147900941014     C     ARBCBO        LOOKUP    T3A(X)                                 26
148000941014     C   26              MOVEL     D3A(X)        DS3A
148100941014     C                   MOVEL     §3ATB1        PORTO                          PORTO DELL
148200941014      * ASSEGNATO : PRENDO DATI TASSAZIONE 1° BOLLA
148300941018     C                   MOVE      *ZEROS        AR6NFT
148400941018     C                   MOVE      *ZEROS        AR6DFT
148500941018     C                   MOVE      *ZEROS        AR6IFT
148600991011     C                   MOVE      *ZEROS        AR6KSC
148700941014     C     PORTO         IFEQ      'A'
148800991011     C                   MOVEL     '1'           KTRC
148900991011     C     KAR6          CHAIN     FIAR601L                           22
149000941014     C                   END
149100941014      * TASSAZ. 2° BOLLA
149200941014     C     §3ATB2        IFNE      *BLANK
149300991011     C                   MOVEL     '2'           KTRC
149400991011     C     KAR6          CHAIN     FIAR601L                           22
149500941014     C                   END
149600941014     C     ARBICA        IFEQ      'S'
149700941014     C     ARBICA        OREQ      'I'
149800911031     C* SE ESISTE 2° BOLLA L'IMP.DELLA FATTURA è SULLA 2°
149900991011     C     PORTO         IFEQ      'A'
150000991025     C     §3ATB2        ORNE      *BLANKS
150100991011     C     AR6DFT        IFNE      0
150200941014     C                   Z-ADD     AR6IFT        §IFT
150300941014     C                   Z-ADD     AR6IFT        §§IFT
150400941014     C                   Z-ADD     AR6NFT        §NFT
150500941014     C                   Z-ADD     AR6DFT        §DFT
150600991011     C     AR6KSC        IFNE      0
150700991011     C                   Z-ADD     AR6KSC        §KSC
150800991011     C                   ELSE
150900991011     C                   Z-ADD     ARBKSC        §KSC
151000941017     C                   END
151100911031     C                   END
151200911105     C* SE INCASSO PARZ. PRENDO QUELLO
151300991011     C                   Z-ADD     0             ARIIMP
151400991019     C                   MOVEL     *BLANKS       ARIDIV
151500991011     C                   MOVEL     'A'           KTIP
151600060127     C* AZZERO TOTALE
151700991019     C                   Z-ADD     0             TOTEUR           19 3
151800991019     C* AZZERO DATI ULTIMO INCASSO
151900991019     C                   Z-ADD     0             ULTINC           19 3
152000991027     C                   MOVEL     'N'           WTRARI
152100991019     C                   MOVEL     *BLANKS       ULTDIV            3
152200991011     C     KARI          CHAIN     FIARI01L                           22
152300991011     C* LOOP FINO A CHE NON TROVO ARIIMP
152400991011     C     *IN22         DOWEQ     '0'
152500991027     C                   MOVEL     'S'           WTRARI
152600991019     C                   ADD       ARIIPP        TOTEUR
152700991019     C* MEMORIZZO DATI ULTIMO INCASSO
152800991019     C     ARIIMP        IFNE      0
152900991019     C                   Z-ADD     ARIIMP        ULTINC
153000991019     C                   MOVE      ARIDIV        ULTDIV
153100991019     C                   END
153200991019     C     KARI          READE     FIARI01L                               22
153300991011     C                   END
153400991027     C* SE ESISTE FIARI IL TIPO INCASSO PARZIALE E' IMPOSTATO E
153500991027     C* NON ESISTE PROGGRESSIVO INCASSATO > 0 IMPOSTO 0 NEL TOTALE
153600991027     C* FATTURA
153700991027     C     ULTINC        IFEQ      0
153800991027     C     ARBFIP        IFEQ      'A'
153900991027     C     WTRARI        ANDEQ     'S'
154000991027     C     ARBFIP        OREQ      'C'
154100991027     C     WTRARI        ANDEQ     'S'
154200991027     C                   Z-ADD     0             ULTINC
154300991027     C                   MOVE      AR6DIV        ULTDIV
154400991027     C                   END
154500991027     C                   END
154600991011     C*
154700991019     C     ULTINC        IFGT      *ZEROS                                        ARBIFP->
154800991105     C* SE HO CHIUSO CON NOTA DI CREDITO IMPORTO FATTURA = 0
154900991105     C     ARBFIP        OREQ      'A'
155000991105     C     WTRARI        ANDEQ     'S'
155100991105     C     ARBFIP        OREQ      'C'
155200991105     C     WTRARI        ANDEQ     'S'
155300991019     C                   Z-ADD     ULTINC        §IFT
155400991019     C                   MOVEL     ULTDIV        §DIV
155500991011     C                   ELSE                                                    X-ARBIFP
155600991011     C                   Z-ADD     AR6IFT        §IFT
155700991014     C                   MOVEL     AR6DIV        §DIV
155800991011     C                   END
155900060214     C* SCRITTURA AVERE DELL'IMPORTO INCASSO ASSEGNATO
156000911104     C     §IFT          IFGT      0
156100991015     C                   EXSR      WRTASS
156200991015     C                   EXSR      WRTMOV
156300991112     C                   END
156400060331     C* RENDO SUBITO VERSABILE l'ASSEGNO
156500060331     C                   SETOFF                                       22
156600060331     C     KAAS          SETLL     CNASS01L
156700060331     C     *IN22         DOUEQ     '1'
156800060331     C     KAAS          READE     CNASS01L                               22
156900060331     C     *IN22         IFEQ      '0'
157000060331     C     ASSTPD        ANDEQ     'A'                                          ASSEGNATO
157100060331     C                   MOVE      'T'           ASSFTR
157200060331     C                   MOVE      'V'           ASSFVE
157300060331     C                   EXCEPT    AGGASS
157400060331     C                   ENDIF
157500060331     C                   ENDDO
157600060214     C* PROCEDO ALLA SCRITTURA DELL'ABBUONO
157700911031     C     ARBFIP        IFEQ      'A'
157800991015     C                   EXSR      WRTABB
157900911031     C                   END
158000991021     C*
158100991021     C                   Z-ADD     0             ARIIMP
158200991021     C                   MOVEL     ULTDIV        KDIV
158300991021     C     KARI1         CHAIN     FIARI01L                           22
158400060130     C* AGGIORNO FIARI
158500991011     C     ARIIMP        IFNE      0
158600991011     C                   ADD       ARIIMP        ARIIPP
158700991011     C                   Z-ADD     0             ARIIMP
158800991011     C                   UPDATE    FIARI000
158900991011     C                   END
159000991011     C                   END
159100991011     C                   END
159200941014     C* HO INCASSATO IL C/ASSEGNO   DEVO SCRIVERE FNCAS00T
159300941018     C                   MOVE      *ZEROS        AR9CAS
159400941018     C                   MOVE      *BLANK        AR9TIC
159500941018     C                   MOVE      *BLANK        AR9VCA
159600941014     C     ARBICC        IFEQ      'S'
159700941014     C     ARBICC        OREQ      'I'
159800941014     C     §3AFCA        IFNE      0
159900051114     C     KARB          CHAIN     FIAR901L                           22
160000941014     C                   ENDIF
160100941014     C                   ENDIF
160200941014     C     AR9CAS        IFGT      *ZEROS
160300941014     C                   Z-ADD     §NMI          CASNDT
160400941017     C                   MOVE      AMGUD8        CASDDC
160500060214     C* PRENDO I DATI DELL' ASSEGNO DA CNASS01L, SE NON LO TROVO è CONTANTE
160600931005     C                   SETOFF                                       7122
160700941014     C     KAAS          SETLL     CNASS01L
160800931005     C     *IN22         DOUEQ     '1'
160900941014     C     KAAS          READE     CNASS01L                               22
161000931005     C     *IN22         IFEQ      '0'
161100941014     C     ASSTPD        ANDEQ     'C'                                          CONTRASS
161200931005     C     *IN22         OREQ      '0'
161300941014     C     ASSTPD        ANDEQ     'T'                                          CONTR+ASSE
161400931005     C                   SETON                                        71
161500941214     C     ASSTPI        IFEQ      'M'
161600941214     C                   MOVE      *ZEROS        CASNRG
161700941214     C                   MOVE      *ZEROS        CASDRE
161800941214     C                   ELSE
161900991125     C                   Z-ADD     0             CASNRG
162000991125     C                   MOVEL     VCDUS         CASNRG
162100991125     C                   MOVE      5             CASNRG
162200941214     C                   MOVE      AMGUD8        CASDRE
162300941214     C                   ENDIF
162400941014     C                   MOVEL     ASSTPI        CASTPI
162500941014     C                   MOVEL     ASSTPA        CASTPA
162600941014     C                   MOVE      ASSABI        CASABI
162700941014     C                   MOVE      ASSCAB        CASCAB
162800941014     C                   MOVE      ASSDTE        CASDTE
162900941014     C                   MOVEL     ASSNRA        CASNRA
163000060131     C* FLAG CNASS00F COME  TRASMESSO
163100060327     C                   MOVE      'T'           ASSFTR
163200060324     C                   MOVE      'V'           ASSFVE
163300941014     C                   EXCEPT    AGGASS
163400931005     C                   ENDIF
163500931005     C                   ENDDO
163600991020     C                   MOVE      AR9CAS        CASCAS
163700991020     C                   MOVE      AR9VCA        CASVCA
163800931005     C*
163900931005     C     *IN71         IFEQ      '0'                                          CONTANTE
164000991125     C                   Z-ADD     0             CASNRG
164100991125     C                   MOVEL     VCDUS         CASNRG
164200991125     C                   MOVE      5             CASNRG
164300950103     C                   MOVE      AMGUD8        CASDRE
164400930409      * IN OGNI CASO ANCHE SE NON HO TROVATO L'ASSEGNO
164500930409      * IMPOSTO I FLAG DEL TIPO INCASSO
164600941014     C                   MOVEL     ' '           CASTPI
164700941014     C                   MOVEL     ' '           CASTPA
164800941017     C     AR9TIC        IFNE      *BLANK
164900930409     C                   CLEAR                   DS1A
165000930409     C                   Z-ADD     1             X
165100941017     C     AR9TIC        LOOKUP    T1A(X)                                 22
165200060213     c                   if        *in22
165300060213     C                   MOVEL     D1A(X)        DS1A
165400060213     C     §1AFMB        IFEQ      'M'
165500941014     C                   MOVEL     §1AFMB        CASTPI
165600941019     C                   MOVE      *ZEROS        CASNRG
165700941019     C                   MOVE      *ZEROS        CASDRE
165800931005     C                   ENDIF
165900060213     C     §1AFCA        IFNE      'S'
166000060331     C     §1AFCA        andne     'P'
166100941014     C                   MOVEL     §1AFCA        CASTPA
166200931005     C                   ENDIF
166300060214     c* se incasso c/assegno con POS: DARE di banca AVERE debiti x c/assegni
166400060213     C     §1AFCA        IFeq      'P'
166500060213     c                   exsr      wrtcasp
166600060213     c                   exsr      wrtmovp
166700060213     C                   ENDIF
166800060213     c*
166900060213     C                   ENDIF
167000931005     C                   ENDIF
167100930409      *
167200941014     C                   MOVE      *ZEROS        CASABI
167300941014     C                   MOVE      *ZEROS        CASCAB
167400941014     C                   MOVE      *ZEROS        CASDTE
167500941014     C                   MOVE      *BLANKS       CASNRA
167600931005     C                   ENDIF                                                       *IN71
167700941014     C                   MOVE      §FLE          CASFLE
167800941014     C                   MOVE      ARBLNP        CASLNP
167900941014     C                   MOVE      ARBLNA        CASLNA
168000941014     C                   MOVE      ARBAAS        CASAAS
168100941014     C                   MOVE      ARBNRS        CASNRS
168200941014     C                   MOVE      ARBNSP        CASNSP
168300911104     C                   ADD       1             NRARC             7 0
168400941014     C                   WRITE     FNCAS000
168500911031     C                   END
168600911031     C* DEVO AGGIORNARE LA BOLLA CON IL MANDATO INTROITO
168700941122     C* SE L'ASSEGNATO ERA UN RITORNO ALL'INCASSO, MA NON HO INCASSATO
168800941122     C* TUTTO --> DEVO IMPOSTARE ARBICA='R' RITOTNO ALL'INCASSO DA FARE
168900941122     C     ARBICA        IFEQ      'I'
169000941122     C     ARBFIP        ANDEQ     'I'
169100941122     C                   MOVEL     'R'           ARBICA
169200941122     C                   END
169300020919     C                   Z-ADD     §FLE          ARBIPP
169400020919     C                   MOVE      §NMI          ARBNMI
169500941014     C                   MOVE      AMGUD8        ARBDMI
169600941017      * AZZERO NR. DISTINTA SE C'E' CONS. PARZIALE E NON è ANCORA CONSEGN
169700941014     C     ARBDCP        IFNE      *ZEROS
169800941017     C     ARBDCM        ANDEQ     *ZEROS
169900930510     C                   MOVE      *ZEROS        ARBNDC
170000020919     C                   Z-ADD     0             ARBIFP
170100930510     C                   END
170200131212      *
170300941014     C                   EXCEPT    AG0ARB
170400131212      *
170500100714     c* se sto chiudendo spedizioni di un p.o. in £6 me lo memorizzo
170600100714     c* per poi sottomettere la trasmissione dei dati di consegna
170700100714     c                   if        arbipp <> arbifp and arbifp <> 0
170800100714     c                             and savlna = 0
170900100714     c                   eval      savlna = arbifp
171000100714     c                   end
171100911031     C* DEVO VEDERE SE ESISTE 3 BOLLA X RIMBORSI DANNI
171200990311     C* SE SPEDIZIONE CON CONSEGNA PARZIALE AVENTE BOLLA MAMMA TOLGO
171300990311     C* NUMERO DISTINTA ANCHE DA QUEST'ULTIMA
171400990311     C     ARBDCP        IFNE      *ZEROS
171500990311     C     ARBDCM        ANDEQ     *ZEROS
171600990311     C     KARB          CHAIN     FNLBL01L                           35
171700990311     C     *IN35         IFEQ      '0'
171800990311     C     LBLLAN        ANDEQ     LBLLAP
171900990311     C     KLBL          CHAIN     FNARB01L                           35
172000990311     C     *IN35         IFEQ      '0'
172100990311     C     ARBDCM        ANDEQ     *ZEROS
172200990311     C     ARBCCA        ANDNE     '2'
172300990311     C     ARBCCA        ANDNE     '6'
172400990311     C                   MOVE      *ZEROS        ARBNDC
172500020919     C                   Z-ADD     0             ARBIFP
172600990311     C                   EXCEPT    AGGMAM
172700990311     C                   END
172800990311     C                   END
172900990311     C                   END
173000911031     C                   END
173100911104     C                   END
173200060214     C* REG. DARE incassi da attribuire x TOTALE INCASSI P/ASSEGNATI
173300060213     C     §paseu        IFGT      0
173400991112     C                   EXSR      TOTASS
173500991015     C                   EXSR      WRTMOV
173600911105     C                   END
173700060214     C* ESEGUO REGISTRAZIONE ABBUONO COMPLESSIVO
173800911031     C     VABB          IFNE      *ZEROS
173900991015     C                   EXSR      TOTABB
174000991018     C                   EXSR      WRTMOV
174100911031     C* REG. CONTROP. ABB.OPERATORE = C/INCASSI ATTRIB.
174200991015     C                   EXSR      REGABB
174300991018     C                   EXSR      WRTMOV
174400911031     C                   END
174500060214      * REGISTRO DARE incassi d'attribuire
174600991020     C     CTBCAE        IFNE      *ZEROS
174700991015     C                   EXSR      WRTCAS
174800991018     C                   EXSR      WRTMOV
174900060214     C* AVERE TOTALE debiti x c/assegni
175000991015     C                   EXSR      WRTCAI
175100991018     C                   EXSR      WRTMOV
175200991015     C                   END
175300060214      * REGISTRO AVERE prepagati
175400060131     C     §preeu        IFNE      *ZEROS
175500060131     C                   EXSR      WRTpre
175600060214     C* DARE TOTALE incassi d'attribuire
175700060131     C                   EXSR      WRTpreI
175800060213     C                   EXSR      WRTMOV
175900060131     C                   END
176000060214     c* REGISTRAZIONI POS
176100060213     c                   exsr      aggiorp
176200060214     C* STAMPO IL MANDATO DI INTROITO
176300060214     c                   eval      §paseut = §paseu + §paseup
176400060214     c                   eval      §preeut = §preeu + §preeup
176500060214     c                   eval      ctbcaet = ctbcae + ctbcaep
176600991118     C                   EXCEPT    STACTB
176700991015     C*
176800911031     C                   ENDSR
176900060213      *-----------------------------------------------------***********
177000060214      * AGGIORNAMENTO ARCHIVI x pos sul conto BANCA
177100060213      *-----------------------------------------------------***********
177200060213     C     AGGIORp       BEGSR
177300060213     C*
177400060213     C     KNMI          SETLL     FNARB71L
177500060213     C     *IN21         DOUEQ     '1'
177600131212      **
177700131212     c     ritenta_p     tag
177800131212     C     KNMI          READE(e)  FNARB71L                               21
177900131212      **
178000131212      *  se il record di ARB è allocato da qualcuno
178100131212      **
178200131212     c                   if        %Error
178300131212      * se il record è allocato mando messaggio info all'utente che alloca il rcd
178400131212     c                   clear                   trul82ds
178500131212     c                   eval      ul82§rrn = arb_nrr
178600131212     c                   eval      ul82§fil = 'FNARB00F'
178700131212     c                   eval      ul82§win = 'S'
178800131212     c                   eval      ul82§f7  = 'S'
178900131212     c                   eval      ul82§num = 2
179000131212     c                   eval      ul82§att = 2
179100131212     c                   eval      ul82§mss = 'Si sta bloccando la << Chiusura '
179200131212     c                             + ' Operatore. >> ' +
179300131212     c                             ' SI PREGA DI USCIRE dal lavoro!'
179400131213     c                   Eval      UL82§msw = 'ATTENZIONE La Chiusura Operatore-
179500131213     c                              è BLOCCATA F12 x Riprovare'
179600131212      *
179700131212      * chiamo il pgm che manda il messaggio info all'utente
179800131212     c                   call(e)   'TRUL82R'
179900131212     c                   parm                    trul82ds
180000131212      *
180100131213      *  Deve sempre rileggere il record  sia x provare se ancora allocato
180200131213      *  che se liberato dall'altro lavoro andrebbe avanti facendo un aggiornamento
180300131213      *  senza lettura del record.
180400131212     c                   goto      ritenta_p
180500131212     c                   end
180600131212      **  Trovato
180700131212      *
180800060213     C     *IN21         IFEQ      '0'
180900060213     C* HO INCASSATO  FATTURA
181000060213     C                   MOVE      *ZEROS        §IFT
181100060213     C                   MOVE      *ZEROS        §§IFT
181200060214     c* guardo il tipo incasso perchè devo escludere quelli NON pos
181300060213     C     ARBMIF        IFNE      *BLANK                                        ARBMIF->
181400060213     C                   CLEAR                   DS1A
181500060213     C                   Z-ADD     1             X
181600060213     C     ARBMIF        LOOKUP    T1A(X)                                 22
181700060213     c                   if        *in22
181800060213     C                   MOVEL     D1A(X)        DS1A
181900060213     c                   if        §1afca <>'P'
182000060213     c                   iter
182100060213     c                   end
182200060213     c                   end
182300060213     c                   end
182400060213      * ANALIZZO LA BOLLA
182500060213     C                   CLEAR                   DS3A
182600060213     C                   Z-ADD     1             X
182700060213     C     ARBCBO        LOOKUP    T3A(X)                                 26
182800060213     C   26              MOVEL     D3A(X)        DS3A
182900060213     C                   MOVEL     §3ATB1        PORTO                          PORTO DELL
183000060213      * ASSEGNATO : PRENDO DATI TASSAZIONE 1° BOLLA
183100060213     C                   MOVE      *ZEROS        AR6NFT
183200060213     C                   MOVE      *ZEROS        AR6DFT
183300060213     C                   MOVE      *ZEROS        AR6IFT
183400060213     C                   MOVE      *ZEROS        AR6KSC
183500060213     C     PORTO         IFEQ      'A'
183600060213     C                   MOVEL     '1'           KTRC
183700060213     C     KAR6          CHAIN     FIAR601L                           22
183800060213     C                   END
183900060213      * TASSAZ. 2° BOLLA
184000060213     C     §3ATB2        IFNE      *BLANK
184100060213     C                   MOVEL     '2'           KTRC
184200060213     C     KAR6          CHAIN     FIAR601L                           22
184300060213     C                   END
184400060213     C     ARBICA        IFEQ      'S'
184500060213     C     ARBICA        OREQ      'I'
184600060213     C* SE ESISTE 2° BOLLA L'IMP.DELLA FATTURA è SULLA 2°
184700060213     C     PORTO         IFEQ      'A'
184800060213     C     §3ATB2        ORNE      *BLANKS
184900060213     C     AR6DFT        IFNE      0
185000060213     C                   Z-ADD     AR6IFT        §IFT
185100060213     C                   Z-ADD     AR6IFT        §§IFT
185200060213     C                   Z-ADD     AR6NFT        §NFT
185300060213     C                   Z-ADD     AR6DFT        §DFT
185400060213     C     AR6KSC        IFNE      0
185500060213     C                   Z-ADD     AR6KSC        §KSC
185600060213     C                   ELSE
185700060213     C                   Z-ADD     ARBKSC        §KSC
185800060213     C                   END
185900060213     C                   END
186000060213     C* SE INCASSO PARZ. PRENDO QUELLO
186100060213     C                   Z-ADD     0             ARIIMP
186200060213     C                   MOVEL     *BLANKS       ARIDIV
186300060213     C                   MOVEL     'A'           KTIP
186400060213     C* AZZERO TOTALE
186500060213     C                   Z-ADD     0             TOTEUR           19 3
186600060213     C* AZZERO DATI ULTIMO INCASSO
186700060213     C                   Z-ADD     0             ULTINC           19 3
186800060213     C                   MOVEL     'N'           WTRARI
186900060213     C                   MOVEL     *BLANKS       ULTDIV            3
187000060213     C     KARI          CHAIN     FIARI01L                           22
187100060213     C* LOOP FINO A CHE NON TROVO ARIIMP
187200060213     C     *IN22         DOWEQ     '0'
187300060213     C                   MOVEL     'S'           WTRARI
187400060213     C                   ADD       ARIIPP        TOTEUR
187500060213     C* MEMORIZZO DATI ULTIMO INCASSO
187600060213     C     ARIIMP        IFNE      0
187700060213     C                   Z-ADD     ARIIMP        ULTINC
187800060213     C                   MOVE      ARIDIV        ULTDIV
187900060213     C                   END
188000060213     C     KARI          READE     FIARI01L                               22
188100060213     C                   END
188200060213     C* SE ESISTE FIARI IL TIPO INCASSO PARZIALE E' IMPOSTATO E
188300060213     C* NON ESISTE PROGGRESSIVO INCASSATO > 0 IMPOSTO 0 NEL TOTALE
188400060213     C* FATTURA
188500060213     C     ULTINC        IFEQ      0
188600060213     C     ARBFIP        IFEQ      'A'
188700060213     C     WTRARI        ANDEQ     'S'
188800060213     C     ARBFIP        OREQ      'C'
188900060213     C     WTRARI        ANDEQ     'S'
189000060213     C                   Z-ADD     0             ULTINC
189100060213     C                   MOVE      AR6DIV        ULTDIV
189200060213     C                   END
189300060213     C                   END
189400060213     C*
189500060213     C     ULTINC        IFGT      *ZEROS                                        ARBIFP->
189600060213     C* SE HO CHIUSO CON NOTA DI CREDITO IMPORTO FATTURA = 0
189700060213     C     ARBFIP        OREQ      'A'
189800060213     C     WTRARI        ANDEQ     'S'
189900060213     C     ARBFIP        OREQ      'C'
190000060213     C     WTRARI        ANDEQ     'S'
190100060213     C                   Z-ADD     ULTINC        §IFT
190200060213     C                   MOVEL     ULTDIV        §DIV
190300060213     C                   ELSE                                                    X-ARBIFP
190400060213     C                   Z-ADD     AR6IFT        §IFT
190500060213     C                   MOVEL     AR6DIV        §DIV
190600060213     C                   END
190700060213     c* DARE  di BANCA AVERE di INCASSO ASSEGNATO
190800060213     C     §IFT          IFGT      0
190900060213     C                   EXSR      WRTASSp
191000060213     C                   END
191100060331     C* RENDO SUBITO VERSABILE l'ASSEGNO
191200060331     C                   SETOFF                                       22
191300060331     C     KAAS          SETLL     CNASS01L
191400060331     C     *IN22         DOUEQ     '1'
191500060331     C     KAAS          READE     CNASS01L                               22
191600060331     C     *IN22         IFEQ      '0'
191700060331     C     ASSTPD        ANDEQ     'A'                                          ASSEGNATO
191800060331     C                   MOVE      'T'           ASSFTR
191900060331     C                   MOVE      'V'           ASSFVE
192000060331     C                   EXCEPT    AGGASS
192100060331     C                   ENDIF
192200060331     C                   ENDDO
192300060213     C* REG. ABBUONO SU CLIENTE
192400060213     C     ARBFIP        IFEQ      'A'
192500060214     C                   EXSR      WRTABBp
192600060213     C                   END
192700060213     C*
192800060213     C                   Z-ADD     0             ARIIMP
192900060213     C                   MOVEL     ULTDIV        KDIV
193000060213     C     KARI1         CHAIN     FIARI01L                           22
193100060213     C* AGGIORNO FIARI
193200060213     C     ARIIMP        IFNE      0
193300060213     C                   ADD       ARIIMP        ARIIPP
193400060213     C                   Z-ADD     0             ARIIMP
193500060213     C                   UPDATE    FIARI000
193600060213     C                   END
193700060213     C                   END
193800060213     C                   END
193900060213     C* HO INCASSATO IL C/ASSEGNO   DEVO SCRIVERE FNCAS00T
194000060213     C                   MOVE      *ZEROS        AR9CAS
194100060213     C                   MOVE      *BLANK        AR9TIC
194200060213     C                   MOVE      *BLANK        AR9VCA
194300060213     C     ARBICC        IFEQ      'S'
194400060213     C     ARBICC        OREQ      'I'
194500060213     C     §3AFCA        IFNE      0
194600060213     C     KARB          CHAIN     FIAR901L                           22
194700060213     C                   ENDIF
194800060213     C                   ENDIF
194900060213     C     AR9CAS        IFGT      *ZEROS
195000060213     C                   Z-ADD     §NMI          CASNDT
195100060213     C                   MOVE      AMGUD8        CASDDC
195200060214     C* PRENDO I DATI DELL' ASSEGNO DA CNASS01L, SE NON LO TROVO è CONTANTE
195300060213     C                   SETOFF                                       7122
195400060213     C     KAAS          SETLL     CNASS01L
195500060213     C     *IN22         DOUEQ     '1'
195600060213     C     KAAS          READE     CNASS01L                               22
195700060213     C     *IN22         IFEQ      '0'
195800060213     C     ASSTPD        ANDEQ     'C'                                          CONTRASS
195900060213     C     *IN22         OREQ      '0'
196000060213     C     ASSTPD        ANDEQ     'T'                                          CONTR+ASSE
196100060213     C                   SETON                                        71
196200060213     C     ASSTPI        IFEQ      'M'
196300060213     C                   MOVE      *ZEROS        CASNRG
196400060213     C                   MOVE      *ZEROS        CASDRE
196500060213     C                   ELSE
196600060213     C                   Z-ADD     0             CASNRG
196700060213     C                   MOVEL     VCDUS         CASNRG
196800060213     C                   MOVE      5             CASNRG
196900060213     C                   MOVE      AMGUD8        CASDRE
197000060213     C                   ENDIF
197100060213     C                   MOVEL     ASSTPI        CASTPI
197200060213     C                   MOVEL     ASSTPA        CASTPA
197300060213     C                   MOVE      ASSABI        CASABI
197400060213     C                   MOVE      ASSCAB        CASCAB
197500060213     C                   MOVE      ASSDTE        CASDTE
197600060213     C                   MOVEL     ASSNRA        CASNRA
197700060213     C* FLAG CNASS00F COME  TRASMESSO
197800060327     C                   MOVE      'T'           ASSFTR
197900060324     C                   MOVE      'V'           ASSFVE
198000060213     C                   EXCEPT    AGGASS
198100060213     C                   ENDIF
198200060213     C                   ENDDO
198300060213     C                   MOVE      AR9CAS        CASCAS
198400060213     C                   MOVE      AR9VCA        CASVCA
198500060213     C*
198600060213     C     *IN71         IFEQ      '0'                                          CONTANTE
198700060213     C                   Z-ADD     0             CASNRG
198800060213     C                   MOVEL     VCDUS         CASNRG
198900060213     C                   MOVE      5             CASNRG
199000060213     C                   MOVE      AMGUD8        CASDRE
199100060213      * IN OGNI CASO ANCHE SE NON HO TROVATO L'ASSEGNO
199200060213      * IMPOSTO I FLAG DEL TIPO INCASSO
199300060213     C                   MOVEL     ' '           CASTPI
199400060213     C                   MOVEL     ' '           CASTPA
199500060213     C     AR9TIC        IFNE      *BLANK
199600060213     C                   CLEAR                   DS1A
199700060213     C                   Z-ADD     1             X
199800060213     C     AR9TIC        LOOKUP    T1A(X)                                 22
199900060213     c                   if        *in22
200000060213     C                   MOVEL     D1A(X)        DS1A
200100060213     C     §1AFMB        IFEQ      'M'
200200060213     C                   MOVEL     §1AFMB        CASTPI
200300060213     C                   MOVE      *ZEROS        CASNRG
200400060213     C                   MOVE      *ZEROS        CASDRE
200500060213     C                   ENDIF
200600060213     C     §1AFCA        IFNE      'S'
200700060331     C     §1AFCA        andne     'P'
200800060213     C                   MOVEL     §1AFCA        CASTPA
200900060213     C                   ENDIF
201000060214     c* se incasso POS: DARE di banca
201100060213     C     §1AFCA        IFeq      'P'
201200060213     c                   exsr      wrtcasp
201300060213     c                   exsr      wrtmovp
201400060213     C                   ENDIF
201500060213     c*
201600060213     C                   ENDIF
201700060213     C                   ENDIF
201800060213      *
201900060213     C                   MOVE      *ZEROS        CASABI
202000060213     C                   MOVE      *ZEROS        CASCAB
202100060213     C                   MOVE      *ZEROS        CASDTE
202200060213     C                   MOVE      *BLANKS       CASNRA
202300060213     C                   ENDIF                                                       *IN71
202400060213     C                   MOVE      §FLE          CASFLE
202500060213     C                   MOVE      ARBLNP        CASLNP
202600060213     C                   MOVE      ARBLNA        CASLNA
202700060213     C                   MOVE      ARBAAS        CASAAS
202800060213     C                   MOVE      ARBNRS        CASNRS
202900060213     C                   MOVE      ARBNSP        CASNSP
203000060213     C                   ADD       1             NRARC             7 0
203100060213     C* SCRIVO RECORD DA TRASMETTERE
203200060213     C                   WRITE     FNCAS000
203300060213     C                   END
203400060213     C*
203500060213     C* DEVO AGGIORNARE LA BOLLA CON IL MANDATO INTROITO
203600060213     C* SE L'ASSEGNATO ERA UN RITORNO ALL'INCASSO, MA NON HO INCASSATO
203700060213     C* TUTTO --> DEVO IMPOSTARE ARBICA='R' RITOTNO ALL'INCASSO DA FARE
203800060213     C     ARBICA        IFEQ      'I'
203900060213     C     ARBFIP        ANDEQ     'I'
204000060213     C                   MOVEL     'R'           ARBICA
204100060213     C                   END
204200060213     C                   Z-ADD     §FLE          ARBIPP
204300060213     C                   MOVE      §NMI          ARBNMI
204400060213     C                   MOVE      AMGUD8        ARBDMI
204500060213      * AZZERO NR. DISTINTA SE C'E' CONS. PARZIALE E NON è ANCORA CONSEGN
204600060213     C     ARBDCP        IFNE      *ZEROS
204700060213     C     ARBDCM        ANDEQ     *ZEROS
204800060213     C                   MOVE      *ZEROS        ARBNDC
204900060213     C                   Z-ADD     0             ARBIFP
205000060213     C                   END
205100060213      *
205200060213     C                   EXCEPT    AG0ARB
205300100714     c* se sto chiudendo spedizioni di un p.o. in £6 me lo memorizzo
205400100714     c* per poi sottomettere la trasmissione dei dati di consegna
205500100714     c                   if        arbipp <> arbifp and arbifp <> 0
205600100714     c                             and savlna = 0
205700100714     c                   eval      savlna = arbifp
205800100714     c                   end
205900060213     C* DEVO VEDERE SE ESISTE 3 BOLLA X RIMBORSI DANNI
206000060213     C* SE SPEDIZIONE CON CONSEGNA PARZIALE AVENTE BOLLA MAMMA TOLGO
206100060213     C* NUMERO DISTINTA ANCHE DA QUEST'ULTIMA
206200060213     C     ARBDCP        IFNE      *ZEROS
206300060213     C     ARBDCM        ANDEQ     *ZEROS
206400060213     C     KARB          CHAIN     FNLBL01L                           35
206500060213     C     *IN35         IFEQ      '0'
206600060213     C     LBLLAN        ANDEQ     LBLLAP
206700060213     C     KLBL          CHAIN     FNARB01L                           35
206800060213     C     *IN35         IFEQ      '0'
206900060213     C     ARBDCM        ANDEQ     *ZEROS
207000060213     C     ARBCCA        ANDNE     '2'
207100060213     C     ARBCCA        ANDNE     '6'
207200060213     C                   MOVE      *ZEROS        ARBNDC
207300060213     C                   Z-ADD     0             ARBIFP
207400060213     C                   EXCEPT    AGGMAM
207500060213     C                   END
207600060213     C                   END
207700060213     C                   END
207800060213     C*
207900060213     C                   END
208000060213     C                   END
208100060214      * REGISTRO TOTALE INCASSI C/Ass. pos in AVERE debiti x contrassegni
208200060213     C     CTBCAEp       IFNE      *ZEROS
208300060213     C                   EXSR      WRTCAIp
208400060213     C                   EXSR      WRTMOVp
208500060213     C                   END
208600060214      * REGISTRO INCASSI prepagati pos - DARE di banca AVERE prepagati
208700060213     C     §preeup       IFNE      *ZEROS
208800060213     C                   EXSR      wrtprep
208900060213     C                   END
209000060213     C*
209100060213     C                   ENDSR
209200991015      *-----------------------------------------------------*
209300991015      * SCRITTURA MOVIMENTI IMPORTO INCASSO TOTALE ASSEGNATI*
209400991015      *-----------------------------------------------------*
209500991015     C     TOTASS        BEGSR
209600991015     C*
209700991015     C                   MOVEL     §FLE          MOVFPN
209800991015     C                   MOVEL     §4OCO1        MOVCOD
209900991015     C                   MOVE      §4OCC1        MOVKCC
210000991015     C                   MOVE      §4OSC1        MOVKSC
210100060214     C                   MOVEL     WCOMO6        MOVNRG
210200060214     C                   MOVE      1             MOVNRG
210300991015     C                   Z-ADD     1             MOVDAR
210400991015     C                   Z-ADD     0             MOVAVE
210500060213     C                   Z-ADD     §paseu        MOVIMM
210600060213     C                   Z-ADD     §paseu        MOVIMD
210700991015     C                   Z-ADD     §NMI          MOVNDC                         MAND.INTRO
210800991015     C                   Z-ADD     AMGUD8        MOVDDC
210900991015     C                   MOVEL     *BLANKS       MOVSRD
211000021114     C                   Z-ADD     §fle          MOVNRP
211100021114     C                   Z-ADD     Amgud8        MOVDRP
211200991015     C                   MOVEL     *BLANKS       MOVSRP
211300991025     C                   Z-ADD     MOVIMM        MOVIPM
211400991025     C                   Z-ADD     MOVIMM        MOVIPD
211500060213     C                   Z-ADD     §paseu        MOVIMM
211600991019     C                   Z-ADD     §PASEU        MOVIMD
211700991019     C                   MOVEL     *BLANKS       MOVDES
211800060216     C                   MOVE      kci           MOVCCP
211900060216     C                   MOVE      *blanks       MOVSCP
212000991015     C*
212100991015     C                   ENDSR
212200060131      *-----------------------------------------------------*
212300060131      * calcola incassi prepagati da chiusura distinta      *
212400060131      *-----------------------------------------------------*
212500060131     C     wrtpre        BEGSR
212600060131     c     kinc          setll     fiinc02l
212700060131     c                   do        *hival
212800060131     c     kinc          reade     fiinc02l
212900060131     c                   if        %eof(fiinc02l)
213000060131     c                   leave
213100060131     c                   end
213200060210     c                   if        inctii <> ' '
213300060210     c                   CLEAR                   DS1A
213400060210     C                   Z-ADD     1             X
213500060210     C     inctii        LOOKUP    T1A(X)                                 22
213600060210     c                   if        *in22
213700060210     C                   MOVEL     D1A(X)        DS1A
213800060210     c* solo se tipo incasso non pos
213900060210     c                   if        §1afca = 'P'
214000060210     c                   iter
214100060210     c                   end
214200060210     c                   end
214300060210     c                   end
214400060131     c* solo tipo documento prepagati, solo da chiusura distinta
214500060131     c                   if        inctpd <>'P' or incchd <>'S'
214600060131     c                   iter
214700060131     c                   end
214800060131     C* UNITA' : PRIME 3 CFR CLIENTE
214900060131     c     kspear6       chain     fiar601l
215000060131     c                   if        %found(fiar601l)
215100060131     c     kspeblp       chain     fnblp01l
215200060131     c                   if        %found(fnblp01l)
215300060131     C                   IF        ar6KSC <> 0
215400060131     C                   Z-ADD     ar6ksc        §ksc
215500060131     C                   ELSE
215600060131     C                   Z-ADD     blpKSC        §ksc
215700060131     C                   END
215800060131     c                   end
215900060131     c                   end
216000060131     C                   MOVEL     §FLE          MOVFPN
216100060213     C                   MOVEL     WCOMO6        movnrg
216200060213     C                   move      6             MOVNRG
216300060213     C                   MOVEL     §4OCO6        MOVCOD
216400060131     C                   MOVE      *ZEROS        MOVKCC
216500060131     C                   MOVE      *ZEROS        MOVKSC
216600060131     C                   MOVE      KCI           MOVKCC
216700060131     C                   MOVE      §KSC          MOVKSC
216800060131     C                   Z-ADD     1             MOVAVE
216900060131     C                   Z-ADD     0             MOVDAR
217000060131     C                   Z-ADD     incimp        MOVIMM
217100060216     C                   Z-ADD     movimm        MOVIMD
217200060216     C                   Z-ADD     §NMI          MOVNDC                         NR.MAND.IN
217300060216     C                   Z-ADD     AMGUD8        MOVDDC
217400060131     C                   MOVEL     *BLANKS       MOVSRD
217500060131     C                   MOVE      incdft        MOVDRP
217600060131     C                   Z-ADD     incnft        MOVNRP
217700060131     C                   MOVEL     incfiv        MOVSRP
217800060131     C                   MOVEL     incfiv        wuni
217900060131     C                   Z-ADD     MOVIMM        MOVIPM
218000060131     C                   Z-ADD     MOVIMD        MOVIPD
218100060131     C                   MOVEL     blpRSD        MOVDES
218200060131     C                   MOVE      §4OCC1        MOVCCP
218300060131     C                   MOVE      §4OSC1        MOVSCP
218400060213     C                   EXSR      WRTMOV
218500060131     c* aggiorno il file degli incassi
218600060131     c                   move      §fle          INCFLE
218700060131     c                   move      vcdus         INCCUC
218800060131     c                   MOVE      §NMI          INCNMI
218900060131     c                   MOVE      AMGUD8        INCDMI
219000060131     c                   MOVEL     '201'         INCSOCIETA
219100060131     c                   MOVEL     'CG'          INCCTB
219200060131     c                   move      AMGUD8        INCDTREG
219300060214     c                   Z-ADD     movnrg        INCNRREG
219400060131     c                   MOVEL     'COGE'        INCSERREG
219500060131     c                   update    fiinc000
219600060215     c* scrivo abbuono nel caso ci sia differenza tra fattura e incassato
219700060215     c                   if        INCIMPDIF <> 0
219800060215     c                   exsr      wrtabbpre
219900060215     c                   end
220000060131     c* aggiorno il file degli assegni
220100060131     c                   if        INCTPA <> ' ' and INCTPI <> 'M'
220200060131     c     kspeass       chain     cnass01l
220300060131     c                   if        %found(cnass01l)
220400060131     C* FLAG CNASS00F COME  TRASMESSO
220500060327     C                   MOVE      'T'           ASSFTR
220600060324     C                   MOVE      'V'           ASSFVE
220700060131     C                   EXCEPT    AGGASS
220800060131     c                   end
220900060131     c                   end
221000060131     c                   enddo
221100060131     C                   ENDSR
221200060131      *-----------------------------------------------------*
221300060131      * SCRITTURA MOVIMENTI IMPORTO INCASSO TOTALE prepagati*
221400060131      *-----------------------------------------------------*
221500060131     C     wrtprei       BEGSR
221600060131     C*
221700060131     C                   MOVEL     §FLE          MOVFPN
221800060213     C                   MOVEL     §4OCO6        MOVCOD
221900060131     C                   MOVE      §4OCC1        MOVKCC
222000060131     C                   MOVE      §4OSC1        MOVKSC
222100060214     C                   MOVEL     WCOMO6        MOVNRG
222200060214     C                   MOVE      6             MOVNRG
222300060131     C                   Z-ADD     1             MOVDAR
222400060131     C                   Z-ADD     0             MOVAVE
222500060131     C                   Z-ADD     §preeu        MOVIMM
222600060215     C                   Z-ADD     movimm        MOVIMD
222700060131     C                   Z-ADD     §NMI          MOVNDC                         MAND.INTRO
222800060131     C                   Z-ADD     AMGUD8        MOVDDC
222900060131     C                   MOVEL     *BLANKS       MOVSRD
223000060131     C                   Z-ADD     §fle          MOVNRP
223100060131     C                   Z-ADD     Amgud8        MOVDRP
223200060131     C                   MOVEL     *BLANKS       MOVSRP
223300060131     C* IMPOSTO  NEL TOTALE PARTITA IL TOTALE IN MONETA CORRENTE
223400060215     C                   Z-ADD     movimm        MOVIPM
223500060215     C                   Z-ADD     movimd        MOVIPD
223600060131     C* ABBLENCO LA DESCRIZIONE
223700060131     C                   MOVEL     *BLANKS       MOVDES
223800060216     C                   MOVE      kci           MOVCCP
223900060216     C                   MOVE      *blanks       MOVSCP
224000060131     C*
224100060131     C                   ENDSR
224200060213      *-----------------------------------------------------*
224300060215      * DARE di BANCA avere di prepagati in dettaglio       *
224400060213      *-----------------------------------------------------*
224500060213     C     wrtprep       BEGSR
224600060213     c     kinc          setll     fiinc02l
224700060213     c                   do        *hival
224800060213     c     kinc          reade     fiinc02l
224900060213     c                   if        %eof(fiinc02l)
225000060213     c                   leave
225100060213     c                   end
225200060213     c                   if        inctii <> ' '
225300060213     c                   CLEAR                   DS1A
225400060213     C                   Z-ADD     1             X
225500060213     C     inctii        LOOKUP    T1A(X)                                 22
225600060213     c                   if        *in22
225700060213     C                   MOVEL     D1A(X)        DS1A
225800060213     c* solo se tipo incasso non pos
225900060213     c                   if        §1afca <>'P'
226000060213     c                   iter
226100060213     c                   end
226200060213     c                   end
226300060213     c                   end
226400060213     c* solo tipo documento prepagati, solo da chiusura distinta
226500060213     c                   if        inctpd <>'P' or incchd <>'S'
226600060213     c                   iter
226700060213     c                   end
226800060213     C* UNITA' : PRIME 3 CFR CLIENTE
226900060213     c     kspear6       chain     fiar601l
227000060213     c                   if        %found(fiar601l)
227100060213     c     kspeblp       chain     fnblp01l
227200060213     c                   if        %found(fnblp01l)
227300060213     C                   IF        ar6KSC <> 0
227400060213     C                   Z-ADD     ar6ksc        §ksc
227500060213     C                   ELSE
227600060213     C                   Z-ADD     blpKSC        §ksc
227700060213     C                   END
227800060213     c                   end
227900060213     c                   end
228000060213     C                   MOVEL     §FLE          MOVFPN
228100060213     C                   MOVEL     WCOMO6        movnrg
228200060213     C                   move      6             MOVNRG
228300060213     C                   MOVEL     §4OCO7        MOVCOD
228400060213     C                   MOVE      *ZEROS        MOVKCC
228500060213     C                   MOVE      *ZEROS        MOVKSC
228600060213     C                   MOVE      KCI           MOVKCC
228700060213     C                   MOVE      §KSC          MOVKSC
228800060213     C                   Z-ADD     1             MOVAVE
228900060213     C                   Z-ADD     0             MOVDAR
229000060213     C                   Z-ADD     incimp        MOVIMM
229100060215     C                   Z-ADD     movimm        MOVIMD
229200060213     C                   MOVEL     *BLANKS       MOVSRD
229300060213     C                   MOVE      incdft        MOVDRP
229400060213     C                   Z-ADD     incnft        MOVNRP
229500060213     C                   MOVEL     incfiv        MOVSRP
229600060213     C                   MOVEL     incfiv        wuni
229700060213     C                   Z-ADD     MOVIMM        MOVIPM
229800060213     C                   Z-ADD     MOVIMD        MOVIPD
229900060213     C                   MOVEL     blpRSD        MOVDES
230000060213     C                   MOVE      §4OCC7        MOVCCP
230100060213     C                   MOVE      §4OSC7        MOVSCP
230200060213     C                   EXSR      WRTMOVp
230300060213     c* DARE della banca
230400060213     C                   MOVE      §4OCC7        MOVKCC
230500060213     C                   MOVE      §4OSC7        MOVKSC
230600060213     C                   MOVEL     *BLANKS       MOVDES
230700060213     C                   Z-ADD     §NMI          MOVNDC                         NR.MAND.IN
230800060213     C                   Z-ADD     AMGUD8        MOVDDC
230900060213      *
231000060213     C                   Z-ADD     0             MOVNRP
231100060213     C                   Z-ADD     0             MOVDRP
231200060215     C                   Z-ADD     0             MOVIPM
231300060215     C                   Z-ADD     0             MOVIPD
231400060213      *
231500060215     C                   Z-ADD     0             MOVAVE
231600060215     C                   Z-ADD     1             MOVDAR
231700060213     C                   MOVE      KCI           MOVCCP
231800060213     C                   MOVE      §KSC          MOVSCP
231900060215     C                   EXSR      WRTMOVp
232000060213     c* aggiorno il file degli incassi
232100060213     c                   move      §fle          INCFLE
232200060213     c                   move      vcdus         INCCUC
232300060213     c                   MOVE      §NMI          INCNMI
232400060213     c                   MOVE      AMGUD8        INCDMI
232500060213     c                   MOVEL     '201'         INCSOCIETA
232600060213     c                   MOVEL     'CG'          INCCTB
232700060213     c                   move      AMGUD8        INCDTREG
232800060214     c                   Z-ADD     movnrg        INCNRREG
232900060213     c                   MOVEL     'COGE'        INCSERREG
233000060213     c                   update    fiinc000
233100060215     c* scrivo abbuono nel caso ci sia differenza tra fattura e incassato
233200060215     c                   if        INCIMPDIF <> 0
233300060215     c                   exsr      wrtabbprep
233400060215     c                   end
233500060404     c* aggiorno il file degli assegni
233600060404     c                   if        INCTPA <> ' ' and INCTPI <> 'M'
233700060404     c     kspeass       chain     cnass01l
233800060404     c                   if        %found(cnass01l)
233900060404     C* FLAG CNASS00F COME  TRASMESSO
234000060404     C                   MOVE      'T'           ASSFTR
234100060404     C                   MOVE      'V'           ASSFVE
234200060404     C                   EXCEPT    AGGASS
234300060404     c                   end
234400060404     c                   end
234500060213     c                   enddo
234600060213     C                   ENDSR
234700991015      *-----------------------------------------------------*
234800991015      * SCRITTUA MOVIMENTI CONTABILI DI INCASSO ASSEGNATI   *
234900991015      *-----------------------------------------------------*
235000991018     C     WRTASS        BEGSR
235100991015     C*
235200991015     C                   MOVEL     §KSC          WUNI              3 0
235300991021     C                   MOVEL     §FLE          MOVFPN
235400991015     C                   MOVE      *ZEROS        MOVKCC
235500991015     C                   MOVE      *ZEROS        MOVKSC
235600991015     C                   MOVE      KCI           MOVKCC
235700991015     C                   MOVE      §KSC          MOVKSC
235800991015     C                   Z-ADD     1             MOVAVE
235900991015     C                   Z-ADD     0             MOVDAR
236000991015     C                   Z-ADD     §IFT          MOVIMM
236100060214     C                   Z-ADD     movimm        MOVIMD
236200991015     C                   MOVE      ARBDDC        MOVDDC
236300991015     C                   Z-ADD     ARBNDC        MOVNDC                         NR.DIST.PA
236400991015     C                   MOVEL     *BLANKS       MOVSRD
236500991015     C                   MOVE      §DFT          MOVDRP
236600991015     C                   Z-ADD     §NFT          MOVNRP
236700991015     C                   MOVEL     WUNI          MOVSRP
236800991022     C                   Z-ADD     MOVIMM        MOVIPM
236900991117     C                   Z-ADD     MOVIMD        MOVIPD
237000991015     C                   MOVEL     ARBRSD        MOVDES
237100060213     C                   MOVEL     WCOMO6        movnrg
237200060213     C                   MOVE      1             movnrg
237300060214     C                   MOVEL     §4OCO1        MOVCOD
237400060214     C                   MOVE      §4OCC1        MOVCCP
237500060214     C                   MOVE      §4OSC1        MOVSCP
237600991015     C*
237700991015     C                   ENDSR
237800060213      *-----------------------------------------------------*
237900060213      * SCRITTUA MOVIMENTI CONTABILI DI INCASSO ASSEGNATI POS  *
238000060213      *-----------------------------------------------------*
238100060213     C     WRTASSp       BEGSR
238200060213     C*
238300060213     C                   MOVEL     §KSC          WUNI              3 0
238400060213     C                   MOVEL     §FLE          MOVFPN
238500060213     C                   MOVEL     §4OCO7        MOVCOD
238600060213     C                   MOVE      *ZEROS        MOVKCC
238700060213     C                   MOVE      *ZEROS        MOVKSC
238800060213     C                   MOVE      KCI           MOVKCC
238900060213     C                   MOVE      §KSC          MOVKSC
239000060213     C                   Z-ADD     1             MOVAVE
239100060213     C                   Z-ADD     0             MOVDAR
239200060213     C                   Z-ADD     §IFT          MOVIMM
239300060214     C                   Z-ADD     movimm        MOVIMD
239400060213     C                   MOVE      ARBDDC        MOVDDC
239500060213     C                   Z-ADD     ARBNDC        MOVNDC                         NR.DIST.PA
239600060213     C                   MOVEL     *BLANKS       MOVSRD
239700060213     C                   MOVE      §DFT          MOVDRP
239800060213     C                   Z-ADD     §NFT          MOVNRP
239900060213     C                   MOVEL     WUNI          MOVSRP
240000060213     C                   Z-ADD     MOVIMM        MOVIPM
240100060213     C                   Z-ADD     MOVIMD        MOVIPD
240200060213     C                   MOVEL     ARBRSD        MOVDES
240300060213     C                   MOVE      §4OCC7        MOVCCP
240400060213     C                   MOVE      §4OSC7        MOVSCP
240500060214     C                   MOVEL     WCOMO6        movnrg
240600060214     C                   MOVE      1             movnrg
240700060213     c                   exsr      wrtmovp
240800060213     c* DARE della banca
240900060214     C                   MOVE      MOVCCP        MOVKCC
241000060214     C                   MOVE      MOVSCP        MOVKSC
241100060213     C                   MOVEL     *BLANKS       MOVDES
241200060213     C                   Z-ADD     §NMI          MOVNDC                         NR.MAND.IN
241300060213     C                   Z-ADD     AMGUD8        MOVDDC
241400060213     C                   Z-ADD     0             MOVNRP
241500060213     C                   Z-ADD     0             MOVDRP
241600060216     C                   Z-ADD     0             MOVIPM
241700060216     C                   Z-ADD     0             MOVIPD
241800060216     C                   Z-ADD     0             MOVAVE
241900060216     C                   Z-ADD     1             MOVDAR
242000060213     C                   MOVE      KCI           MOVCCP
242100060213     C                   MOVE      §KSC          MOVSCP
242200060215     c                   exsr      wrtmovp
242300060213     C*
242400060213     C                   ENDSR
242500991015      *-----------------------------------------------------*
242600060215      * SCRITTURA MOVIMENTO CONTABILE DI ABBUONO ASSEGNATI
242700991015      *-----------------------------------------------------*
242800991015     C     WRTABB        BEGSR
242900991015     C*
243000010626     C                   MOVEL     §FLE          MOVFPN
243100991015     C                   MOVE      *ZEROS        MOVKCC
243200991015     C                   MOVE      *ZEROS        MOVKSC
243300060214     C* LEGGO FILE FIARI, SOMMO gli importi parz. FRA DI LORO E SOTTRAGGO IL
243400991015     C* TOTALE DALL'IMPORTO DELLA FATTURA --> ABBUONO
243500991015     C                   Z-ADD     0             §IPP
243600991015     C     KARI          CHAIN     FIARI01L                           33
243700991015     C     *IN33         DOWEQ     '0'
243800991015     C                   ADD       ARIIPP        §IPP
243900991015     C                   ADD       ARIIMP        §IPP
244000991015     C     KARI          READE     FIARI01L                               33
244100991015     C                   END
244200991018     C     §§IFT         SUB       §IPP          MOVIMM
244300010626     C     §§IFT         SUB       §IPP          MOVIMD
244400991118     C                   Z-ADD     MOVIMM        WABB             19 3
244500991118     C     WABB          IFLT      0
244600991118     C                   MULT      -1            MOVIMM
244700991118     C                   MULT      -1            MOVIMD
244800991118     C                   MOVE      §4OCC4        MOVKCC
244900991118     C                   MOVE      §4OSC4        MOVKSC
245000991125     C                   MOVEL     §4OCO4        MOVCOD
245100991118     C                   Z-ADD     0             MOVDAR
245200991118     C                   Z-ADD     1             MOVAVE
245300991118     C                   ELSE
245400991118     C                   MOVE      §4OCC3        MOVKCC
245500991118     C                   MOVE      §4OSC3        MOVKSC
245600991125     C                   MOVEL     §4OCO3        MOVCOD
245700991118     C                   Z-ADD     1             MOVDAR
245800991118     C                   Z-ADD     0             MOVAVE
245900991118     C                   END
246000991015     C* CONTROPARTITA: CONTO DEL CLIENTE
246100991019     C                   MOVE      *ZEROS        MOVCCP
246200991019     C                   MOVE      *ZEROS        MOVSCP
246300991015     C                   MOVE      KCI           MOVCCP
246400991015     C                   MOVE      §KSC          MOVSCP
246500991025     C* AZZERO I DATI DI PARTITA PER CONTO ABBUONI
246600991025     C                   Z-ADD     0             MOVIPM
246700991025     C                   Z-ADD     0             MOVIPD
246800991025     C                   Z-ADD     0             MOVDRP
246900991025     C                   Z-ADD     0             MOVNRP
247000991015     C* NON REIMPOSTO DIVISA, CAMBIO, DATA RIF.CAMBIO + RIF.DOCUM.
247100991015     C* + RIF.PARTITA PERCHE' SONO UGUALI A QUELLI DELL'ASSEGNATO
247200060213     C                   MOVEL     WCOMO6        MOVNRG
247300060213     C                   MOVE      1             MOVNRG
247400991015     C                   EXSR      WRTMOV
247500991015     C* REG. SU CONTO ABBUONI PASSIVI: IMPOSTO SOLO DATI DIVERSI
247600991015     C                   MOVE      *ZEROS        MOVKCC
247700991015     C                   MOVE      *ZEROS        MOVKSC
247800991015     C                   MOVE      MOVCCP        MOVKCC
247900991022     C                   MOVE      MOVSCP        MOVKSC
248000991015     C                   MOVE      *ZEROS        MOVCCP
248100991015     C                   MOVE      *ZEROS        MOVSCP
248200060215     C*                  MOVEL     §4OCO1        MOVCOD
248300991118     C* Controllo se abbuono attivo o passivo
248400991118     C     WABB          IFLT      0
248500991118     C                   MOVE      §4OCC4        MOVCCP
248600991118     C                   MOVE      §4OSC4        MOVSCP
248700991118     C                   Z-ADD     1             MOVDAR
248800991118     C                   Z-ADD     0             MOVAVE
248900991118     C                   ELSE
249000991015     C                   MOVE      §4OCC3        MOVCCP
249100991015     C                   MOVE      §4OSC3        MOVSCP
249200991015     C                   Z-ADD     0             MOVDAR
249300991015     C                   Z-ADD     1             MOVAVE
249400991118     C                   END
249500060213     c                   movel     wcomo6        movnrg
249600060213     c                   move      1             movnrg
249700991025     C* REIMPOSTO I DATI PARTITA PER CONTO ASSEGNATI
249800991025     C                   Z-ADD     MOVIMM        MOVIPM
249900010626     C                   Z-ADD     MOVIMD        MOVIPD
250000991025     C                   Z-ADD     AR6DFT        MOVDRP
250100991025     C                   Z-ADD     AR6NFT        MOVNRP
250200991015     C                   EXSR      WRTMOV
250300991015     C*
250400991015     C                   ENDSR
250500060214      *-----------------------------------------------------*
250600060215      * SCRITTURA MOVIMENTO CONTABILE DI ABBUONO ASSEGNATI x POS
250700060214      *-----------------------------------------------------*
250800060214     C     WRTABBp       BEGSR
250900060214     C*
251000060214     C                   MOVEL     §FLE          MOVFPN
251100060214     C                   MOVE      *ZEROS        MOVKCC
251200060214     C                   MOVE      *ZEROS        MOVKSC
251300060214     C* LEGGO FILE FIARI, SOMMO gli importi parz. FRA DI LORO E SOTTRAGGO IL
251400060214     C* TOTALE DALL'IMPORTO DELLA FATTURA --> ABBUONO
251500060214     C                   Z-ADD     0             §IPP
251600060214     C     KARI          CHAIN     FIARI01L                           33
251700060214     C     *IN33         DOWEQ     '0'
251800060214     C                   ADD       ARIIPP        §IPP
251900060214     C                   ADD       ARIIMP        §IPP
252000060214     C     KARI          READE     FIARI01L                               33
252100060214     C                   END
252200060214     C     §§IFT         SUB       §IPP          MOVIMM
252300060214     C     §§IFT         SUB       §IPP          MOVIMD
252400060214     C                   Z-ADD     MOVIMM        WABB             19 3
252500060214     C     WABB          IFLT      0
252600060214     C                   MULT      -1            MOVIMM
252700060214     C                   MULT      -1            MOVIMD
252800060214     C                   MOVE      §4OCC4        MOVKCC
252900060214     C                   MOVE      §4OSC4        MOVKSC
253000060214     C                   MOVEL     §4OCO4        MOVCOD
253100060214     C                   Z-ADD     0             MOVDAR
253200060214     C                   Z-ADD     1             MOVAVE
253300060214     C                   ELSE
253400060214     C                   MOVE      §4OCC3        MOVKCC
253500060214     C                   MOVE      §4OSC3        MOVKSC
253600060214     C                   MOVEL     §4OCO3        MOVCOD
253700060214     C                   Z-ADD     1             MOVDAR
253800060214     C                   Z-ADD     0             MOVAVE
253900060214     C                   END
254000060214     C* CONTROPARTITA: CONTO DEL CLIENTE
254100060214     C                   MOVE      *ZEROS        MOVCCP
254200060214     C                   MOVE      *ZEROS        MOVSCP
254300060214     C                   MOVE      KCI           MOVCCP
254400060214     C                   MOVE      §KSC          MOVSCP
254500060214     C* AZZERO I DATI DI PARTITA PER CONTO ABBUONI
254600060214     C                   Z-ADD     0             MOVIPM
254700060214     C                   Z-ADD     0             MOVIPD
254800060214     C                   Z-ADD     0             MOVDRP
254900060214     C                   Z-ADD     0             MOVNRP
255000060214     C* NON REIMPOSTO DIVISA, CAMBIO, DATA RIF.CAMBIO + RIF.DOCUM.
255100060214     C* + RIF.PARTITA PERCHE' SONO UGUALI A QUELLI DELL'ASSEGNATO
255200060214     C                   MOVEL     WCOMO6        MOVNRG
255300060214     C                   MOVE      1             MOVNRG
255400060214     C                   EXSR      WRTMOVp
255500060214     C* REG. SU CONTO ABBUONI PASSIVI: IMPOSTO SOLO DATI DIVERSI
255600060214     C                   MOVE      *ZEROS        MOVKCC
255700060214     C                   MOVE      *ZEROS        MOVKSC
255800060214     C                   MOVE      MOVCCP        MOVKCC
255900060214     C                   MOVE      MOVSCP        MOVKSC
256000060214     C                   MOVE      *ZEROS        MOVCCP
256100060214     C                   MOVE      *ZEROS        MOVSCP
256200060215     C*                  MOVEL     §4OCO7        MOVCOD
256300060214     C* Controllo se abbuono attivo o passivo
256400060214     C     WABB          IFLT      0
256500060214     C                   MOVE      §4OCC4        MOVCCP
256600060214     C                   MOVE      §4OSC4        MOVSCP
256700060214     C                   Z-ADD     1             MOVDAR
256800060214     C                   Z-ADD     0             MOVAVE
256900060214     C                   ELSE
257000060214     C                   MOVE      §4OCC3        MOVCCP
257100060214     C                   MOVE      §4OSC3        MOVSCP
257200060214     C                   Z-ADD     0             MOVDAR
257300060214     C                   Z-ADD     1             MOVAVE
257400060214     C                   END
257500060214     c                   movel     wcomo6        movnrg
257600060214     c                   move      1             movnrg
257700060214     C* REIMPOSTO I DATI PARTITA PER CONTO ASSEGNATI
257800060214     C                   Z-ADD     MOVIMM        MOVIPM
257900060214     C                   Z-ADD     MOVIMD        MOVIPD
258000060214     C                   Z-ADD     AR6DFT        MOVDRP
258100060214     C                   Z-ADD     AR6NFT        MOVNRP
258200060214     C                   EXSR      WRTMOVp
258300060214     C*
258400060214     C                   ENDSR
258500060215      *-----------------------------------------------------*
258600060215      * SCRITTURA MOVIMENTO CONTABILE DI ABBUONO x prepagati*
258700060215      *-----------------------------------------------------*
258800060215     C     WRTABBpre     BEGSR
258900060215     C*
259000060215     C                   MOVEL     §FLE          MOVFPN
259100060215     C                   MOVE      *ZEROS        MOVKCC
259200060215     C                   MOVE      *ZEROS        MOVKSC
259300060215     C                   eval      MOVIMM = INCIMPDIF
259400060215     C                   eval      MOVIMD = movimm
259500060215     C                   Z-ADD     MOVIMM        WABB             19 3
259600060421     c* ragiona diversamente dall'abbuono ass. (fattura - incasso), nel file
259700060421     c* degli incassi FIINC invece ragiona (incasso - fattura)
259800060421     C     WABB          IFgT      0
259900060215     C                   MOVE      §4OCC4        MOVKCC
260000060215     C                   MOVE      §4OSC4        MOVKSC
260100060215     C                   MOVEL     §4OCO4        MOVCOD
260200060215     C                   Z-ADD     0             MOVDAR
260300060215     C                   Z-ADD     1             MOVAVE
260400060215     C                   ELSE
260500060421     C                   MULT      -1            MOVIMM
260600060421     C                   MULT      -1            MOVIMD
260700060215     C                   MOVE      §4OCC3        MOVKCC
260800060215     C                   MOVE      §4OSC3        MOVKSC
260900060215     C                   MOVEL     §4OCO3        MOVCOD
261000060215     C                   Z-ADD     1             MOVDAR
261100060215     C                   Z-ADD     0             MOVAVE
261200060215     C                   END
261300060215     C* CONTROPARTITA: CONTO DEL CLIENTE
261400060215     C                   MOVE      *ZEROS        MOVCCP
261500060215     C                   MOVE      *ZEROS        MOVSCP
261600060215     C                   MOVE      KCI           MOVCCP
261700060215     C                   MOVE      §KSC          MOVSCP
261800060215     C* AZZERO I DATI DI PARTITA PER CONTO ABBUONI
261900060215     C                   Z-ADD     0             MOVIPM
262000060215     C                   Z-ADD     0             MOVIPD
262100060215     C                   Z-ADD     0             MOVDRP
262200060215     C                   Z-ADD     0             MOVNRP
262300060215     C* NON REIMPOSTO DIVISA, CAMBIO, DATA RIF.CAMBIO + RIF.DOCUM.
262400060215     C* + RIF.PARTITA PERCHE' SONO UGUALI A QUELLI DEL prepagato
262500060215     C                   MOVEL     WCOMO6        MOVNRG
262600060215     C                   MOVE      6             MOVNRG
262700060215     C                   EXSR      WRTMOV
262800060215     C* REG. SU CONTO ABBUONI PASSIVI: IMPOSTO SOLO DATI DIVERSI
262900060215     C                   MOVE      *ZEROS        MOVKCC
263000060215     C                   MOVE      *ZEROS        MOVKSC
263100060215     C                   MOVE      MOVCCP        MOVKCC
263200060215     C                   MOVE      MOVSCP        MOVKSC
263300060215     C                   MOVE      *ZEROS        MOVCCP
263400060215     C                   MOVE      *ZEROS        MOVSCP
263500060215     C*                  MOVEL     §4OCO1        MOVCOD
263600060215     C* Controllo se abbuono attivo o passivo
263700060421     C     WABB          IFGT      0
263800060215     C                   MOVE      §4OCC4        MOVCCP
263900060215     C                   MOVE      §4OSC4        MOVSCP
264000060215     C                   Z-ADD     1             MOVDAR
264100060215     C                   Z-ADD     0             MOVAVE
264200060215     C                   ELSE
264300060215     C                   MOVE      §4OCC3        MOVCCP
264400060215     C                   MOVE      §4OSC3        MOVSCP
264500060215     C                   Z-ADD     0             MOVDAR
264600060215     C                   Z-ADD     1             MOVAVE
264700060215     C                   END
264800060215     c                   movel     wcomo6        movnrg
264900060215     c                   move      6             movnrg
265000060215     C* REIMPOSTO I DATI PARTITA PER CONTO prepagati
265100060215     C                   Z-ADD     MOVIMM        MOVIPM
265200060215     C                   Z-ADD     MOVIMD        MOVIPD
265300060215     C                   Z-ADD     incDFT        MOVDRP
265400060215     C                   Z-ADD     incNFT        MOVNRP
265500060215     C                   EXSR      WRTMOV
265600060215     C*
265700060215     C                   ENDSR
265800060215      *-----------------------------------------------------*
265900060215      * SCRITTURA MOVIMENTO CONTABILE DI ABBUONO x prepagati POS
266000060215      *-----------------------------------------------------*
266100060215     C     WRTABBprep    BEGSR
266200060215     C*
266300060215     C                   MOVEL     §FLE          MOVFPN
266400060215     C                   MOVE      *ZEROS        MOVKCC
266500060215     C                   MOVE      *ZEROS        MOVKSC
266600060215     C                   eval      MOVIMM = INCIMPDIF
266700060215     C                   eval      MOVIMD = movimm
266800060215     C                   Z-ADD     MOVIMM        WABB             19 3
266900060421     C     WABB          IFGT      0
267000060215     C                   MOVE      §4OCC4        MOVKCC
267100060215     C                   MOVE      §4OSC4        MOVKSC
267200060215     C                   MOVEL     §4OCO4        MOVCOD
267300060215     C                   Z-ADD     0             MOVDAR
267400060215     C                   Z-ADD     1             MOVAVE
267500060215     C                   ELSE
267600060421     C                   MULT      -1            MOVIMM
267700060421     C                   MULT      -1            MOVIMD
267800060215     C                   MOVE      §4OCC3        MOVKCC
267900060215     C                   MOVE      §4OSC3        MOVKSC
268000060215     C                   MOVEL     §4OCO3        MOVCOD
268100060215     C                   Z-ADD     1             MOVDAR
268200060215     C                   Z-ADD     0             MOVAVE
268300060215     C                   END
268400060215     C* CONTROPARTITA: CONTO DEL CLIENTE
268500060215     C                   MOVE      *ZEROS        MOVCCP
268600060215     C                   MOVE      *ZEROS        MOVSCP
268700060215     C                   MOVE      KCI           MOVCCP
268800060215     C                   MOVE      §KSC          MOVSCP
268900060215     C* AZZERO I DATI DI PARTITA PER CONTO ABBUONI
269000060215     C                   Z-ADD     0             MOVIPM
269100060215     C                   Z-ADD     0             MOVIPD
269200060215     C                   Z-ADD     0             MOVDRP
269300060215     C                   Z-ADD     0             MOVNRP
269400060215     C* NON REIMPOSTO DIVISA, CAMBIO, DATA RIF.CAMBIO + RIF.DOCUM.
269500060215     C* + RIF.PARTITA PERCHE' SONO UGUALI A QUELLI DELL'ASSEGNATO
269600060215     C                   MOVEL     WCOMO6        MOVNRG
269700060215     C                   MOVE      6             MOVNRG
269800060215     C                   EXSR      WRTMOVp
269900060215     C* REG. SU CONTO ABBUONI PASSIVI: IMPOSTO SOLO DATI DIVERSI
270000060215     C                   MOVE      *ZEROS        MOVKCC
270100060215     C                   MOVE      *ZEROS        MOVKSC
270200060215     C                   MOVE      MOVCCP        MOVKCC
270300060215     C                   MOVE      MOVSCP        MOVKSC
270400060215     C                   MOVE      *ZEROS        MOVCCP
270500060215     C                   MOVE      *ZEROS        MOVSCP
270600060215     C*                  MOVEL     §4OCO7        MOVCOD
270700060215     C* Controllo se abbuono attivo o passivo
270800060421     C     WABB          IFgt      0
270900060215     C                   MOVE      §4OCC4        MOVCCP
271000060215     C                   MOVE      §4OSC4        MOVSCP
271100060215     C                   Z-ADD     1             MOVDAR
271200060215     C                   Z-ADD     0             MOVAVE
271300060215     C                   ELSE
271400060215     C                   MOVE      §4OCC3        MOVCCP
271500060215     C                   MOVE      §4OSC3        MOVSCP
271600060215     C                   Z-ADD     0             MOVDAR
271700060215     C                   Z-ADD     1             MOVAVE
271800060215     C                   END
271900060215     c                   movel     wcomo6        movnrg
272000060215     c                   move      6             movnrg
272100060215     C* REIMPOSTO I DATI PARTITA PER CONTO ASSEGNATI
272200060215     C                   Z-ADD     MOVIMM        MOVIPM
272300060215     C                   Z-ADD     MOVIMD        MOVIPD
272400060215     C                   Z-ADD     incDFT        MOVDRP
272500060215     C                   Z-ADD     incNFT        MOVNRP
272600060215     C                   EXSR      WRTMOVp
272700060215     C*
272800060215     C                   ENDSR
272900991015      *-----------------------------------------------------*
273000991015      * SCRIVO MOVIMENTO CONTABILI X ABBUONO COMPLESSIVO    *
273100991015      *-----------------------------------------------------*
273200991015     C     TOTABB        BEGSR
273300991015     C*
273400991015     C* AGGIUNGO 1 ALL'ULTIMO NR.PROGGRESSIVO DI REG.ATTRIBUITO
273500991015     C* X DETERMINARE IL NUOVO NRG DI REGISTRAZIONE ABBUONI
273600991209     C                   MOVEL     §FLE          MOVFPN
273700060214     C                   MOVEL     WCOMO6        MOVNRG
273800060214     C                   MOVE      2             MOVNRG
273900991125     C                   MOVE      *ZEROS        MOVKCC
274000991125     C                   MOVE      *ZEROS        MOVKSC
274100991118     C     VABB          IFGT      *ZEROS                                        PASSIVO
274200991015     C                   MOVEL     §4OCO3        MOVCOD
274300991015     C                   MOVE      §4OCC3        MOVKCC
274400991015     C                   MOVE      §4OSC3        MOVKSC
274500991015     C                   Z-ADD     1             MOVDAR
274600991015     C                   Z-ADD     0             MOVAVE
274700010620     C                   Z-ADD     VABB          WWABB            19 3
274800991015     C                   ELSE                                                    ATTIVO
274900010620     C     VABB          MULT      -1            WWABB
275000991015     C                   MOVEL     §4OCO4        MOVCOD
275100991015     C                   MOVE      §4OCC4        MOVKCC
275200991015     C                   MOVE      §4OSC4        MOVKSC
275300991015     C                   Z-ADD     0             MOVDAR
275400991015     C                   Z-ADD     1             MOVAVE
275500991015     C                   END
275600010620     C                   Z-ADD     WWABB         MOVIMM
275700010622     C                   Z-ADD     WWABB         MOVIMD
275800991015     C                   Z-ADD     §NMI          MOVNDC
275900991015     C                   Z-ADD     AMGUD8        MOVDDC
276000991015     C                   MOVEL     *BLANKS       MOVSRD
276100991015     C                   Z-ADD     0             MOVNRP
276200991015     C                   Z-ADD     0             MOVDRP
276300991015     C                   MOVEL     *BLANKS       MOVSRP
276400991018     C                   MOVE      §4OCC1        MOVCCP
276500991018     C                   MOVE      §4OSC1        MOVSCP
276600991015     C                   Z-ADD     0             MOVNRP
276700991015     C                   Z-ADD     0             MOVDRP
276800991015     C                   Z-ADD     0             MOVIPM
276900991015     C                   Z-ADD     0             MOVIPD
277000991015     C                   MOVEL     *BLANKS       MOVDES
277100991015     C*
277200991015     C                   ENDSR
277300991015      *-----------------------------------------------------***********
277400991018      * SCRIVO MOVIMENTO CONTABILE ABBUONO                  *  WRTMOV *
277500991015      *-----------------------------------------------------***********
277600991015     C     REGABB        BEGSR
277700991015     C*
277800991118     C     VABB          IFGT      *ZEROS                                        PASSIVO
277900991015     C                   Z-ADD     0             MOVDAR
278000991015     C                   Z-ADD     1             MOVAVE
278100991015     C                   ELSE                                                    ATTIVO
278200991015     C                   Z-ADD     1             MOVDAR
278300991015     C                   Z-ADD     0             MOVAVE
278400991015     C                   END
278500991022     C                   Z-ADD     MOVIMM        MOVIPM
278600991022     C                   Z-ADD     MOVIMD        MOVIPD
278700991015     C                   MOVEL     *BLANKS       MOVDES
278800991015     C*
278900021114     C*                  Z-ADD     ARBNMI        MOVNRP
279000021114     C*                  Z-ADD     ARBDMI        MOVDRP
279100021114     C                   Z-ADD     §fle          MOVNRP
279200021114     C                   Z-ADD     amgud8        MOVDRP
279300991015     C                   MOVEL     *BLANKS       MOVSRP
279400991019     C                   MOVE      *ZEROS        MOVCCP
279500991019     C                   MOVE      *ZEROS        MOVSCP
279600991015     C                   MOVE      MOVKCC        MOVCCP
279700991015     C                   MOVE      MOVKSC        MOVSCP
279800991015     C                   MOVE      §4OCC1        MOVKCC
279900991015     C                   MOVE      §4OSC1        MOVKSC
280000991015     C*
280100991015     C                   ENDSR
280200991015      *-----------------------------------------------------***********
280300991018      * SCRITTURA C/ASSEGNI                                 *  WRTMOV *
280400991015      *-----------------------------------------------------***********
280500991015     C     WRTCAS        BEGSR
280600991015     C*
280700991020     C                   MOVEL     §FLE          MOVFPN
280800060214     C                   MOVEL     WCOMO6        MOVNRG
280900060214     C                   MOVE      5             MOVNRG
281000991015     C                   MOVEL     §4OCO1        MOVCOD
281100991015     C                   MOVE      §4OCC1        MOVKCC
281200991015     C                   MOVE      §4OSC1        MOVKSC
281300991015     C                   MOVEL     *BLANKS       MOVDES
281400991015     C                   Z-ADD     §NMI          MOVNDC
281500991015     C                   Z-ADD     AMGUD8        MOVDDC
281600060214     C                   Z-ADD     CTBCAE        MOVIMM
281700060214     C                   Z-ADD     MOVIMM        MOVIMD
281800991015      *
281900021114     C                   Z-ADD     §fle          MOVNRP
282000021114     C                   Z-ADD     amgud8        MOVDRP
282100991015     C                   Z-ADD     MOVIMM        MOVIPM
282200991118     C                   Z-ADD     MOVIMM        MOVIPD
282300991015     C                   Z-ADD     1             MOVDAR
282400991015     C                   Z-ADD     0             MOVAVE
282500991019     C                   MOVE      *ZEROS        MOVCCP
282600991019     C                   MOVE      *ZEROS        MOVSCP
282700991018     C                   MOVE      §4OCC2        MOVCCP
282800991018     C                   MOVE      §4OSC2        MOVSCP
282900991015     C*
283000991015     C                   ENDSR
283100060213      *-----------------------------------------------------***********
283200060213      * SCRITTURA C/ASSEGNI POS DARE di banca               *  WRTMOV *
283300060213      *-----------------------------------------------------***********
283400060213     C     WRTCASp       BEGSR
283500060213     C*
283600060213     C                   MOVEL     §FLE          MOVFPN
283700060216     C                   MOVEL     WCOMO6        MOVNRG
283800060216     C                   MOVE      5             MOVNRG
283900060213     C                   MOVEL     §4OCO7        MOVCOD
284000060213     C                   MOVE      §4OCC7        MOVKCC
284100060213     C                   MOVE      §4OSC7        MOVKSC
284200060213     C                   MOVEL     *BLANKS       MOVDES
284300060213     C                   Z-ADD     §NMI          MOVNDC
284400060213     C                   Z-ADD     AMGUD8        MOVDDC
284500060213     C                   Z-ADD     ar9cas        MOVIMM
284600060214     C                   Z-ADD     movimm        MOVIMd
284700060213      *
284800060214     C                   Z-ADD     §fle          MOVNRP
284900060214     C                   Z-ADD     amgud8        MOVDRP
285000060214     C                   Z-ADD     movimm        MOVIPM
285100060214     C                   Z-ADD     movimm        MOVIPD
285200060213     C                   Z-ADD     1             MOVDAR
285300060213     C                   Z-ADD     0             MOVAVE
285400060213     C                   MOVE      *ZEROS        MOVCCP
285500060213     C                   MOVE      *ZEROS        MOVSCP
285600060213     C                   MOVE      §4OCC2        MOVCCP
285700060213     C                   MOVE      §4OSC2        MOVSCP
285800060213     C*
285900060213     C                   ENDSR
286000991015      *-----------------------------------------------------*
286100991015      * SCRITTURA C/ASSEGNI D'ATTRIBUIRE                    *
286200991015      *-----------------------------------------------------*
286300991015     C     WRTCAI        BEGSR
286400991015      *
286500991015     C                   MOVE      §4OCC2        MOVKCC
286600991015     C                   MOVE      §4OSC2        MOVKSC
286700991015     C                   MOVEL     §4OCO2        MOVCOD
286800991015     C                   MOVEL     *BLANKS       MOVDES
286900991015     C                   Z-ADD     §NMI          MOVNDC                         NR.MAND.IN
287000991015     C                   Z-ADD     AMGUD8        MOVDDC
287100060216     C                   Z-ADD     ctbcae        MOVIMM
287200060216     C                   Z-ADD     movimm        MOVIMD
287300991015      *
287400991015     C                   Z-ADD     0             MOVNRP
287500991015     C                   Z-ADD     0             MOVDRP
287600060214     C                   Z-ADD     0             MOVipm
287700060214     C                   Z-ADD     0             MOVipd
287800991015      *
287900991015     C                   Z-ADD     1             MOVAVE
288000991015     C                   Z-ADD     0             MOVDAR
288100991018     C                   MOVE      §4OCC1        MOVCCP
288200991018     C                   MOVE      §4OSC1        MOVSCP
288300991015     C*
288400991015     C                   ENDSR
288500060213      *-----------------------------------------------------*
288600060213      * SCRITTURA  conto avere debiti x c/assegni
288700060213      *-----------------------------------------------------*
288800060213     C     WRTCAIp       BEGSR
288900060213      *
289000060214     C                   MOVEL     WCOMO6        MOVNRG
289100060214     C                   MOVE      5             MOVNRG
289200060214     C                   Z-ADD     CTBCAEp       MOVIMD
289300060213     C                   MOVE      §4OCC2        MOVKCC
289400060213     C                   MOVE      §4OSC2        MOVKSC
289500060213     C                   MOVEL     §4OCO2        MOVCOD
289600060213     C                   MOVEL     *BLANKS       MOVDES
289700060213     C                   Z-ADD     §NMI          MOVNDC                         NR.MAND.IN
289800060213     C                   Z-ADD     AMGUD8        MOVDDC
289900060216     C                   Z-ADD     ctbcaep       MOVIMm
290000060216     C                   Z-ADD     movimm        MOVIMd
290100060213      *
290200060213     C                   Z-ADD     0             MOVNRP
290300060213     C                   Z-ADD     0             MOVDRP
290400060214     C                   Z-ADD     0             MOVipm
290500060214     C                   Z-ADD     0             MOVipd
290600060213      *
290700060213     C                   Z-ADD     1             MOVAVE
290800060213     C                   Z-ADD     0             MOVDAR
290900060213     C                   MOVE      §4OCC7        MOVCCP
291000060213     C                   MOVE      §4OSC7        MOVSCP
291100060213     C*
291200060213     C                   ENDSR
291300991015      *-----------------------------------------------------***********
291400991018      * SCRIVO MOVIMENTO CONTABILI                          *  WRTMOV *
291500991015      *-----------------------------------------------------***********
291600991015     C     WRTMOV        BEGSR
291700991015     C*
291800991015     C* INCREMENTO NR. PRG REGISTRAZIONE
291900991015     C                   ADD       1             SUBRIG
292000991015     C                   Z-ADD     SUBRIG        MOVSBR
292100991015     C                   MOVE      KNMUS         MOVNMU
292200991018     C                   MOVE      KNRAZ         MOVRIF
292300991018     C                   MOVEL     §FLE          MOVRIF
292400060214     C                   Z-ADD     movnrg        MOVSBN
292500991015     C                   MOVEL     'T'           MOVFGE
292600010312     C                   MOVEL     '201'         MOVSOC
292700991015     C                   MOVEL     'D'           MOVTMO
292800991015     C                   MOVEL     'CG'          MOVCTB
292900060213     C                   MOVEL     §GEDMC        MOVDIP
293000060213     C                   MOVEL     §GEDMC        MOVDIv
293100991015     C                   Z-ADD     AMGUD8        MOVDRG
293200991015     C                   MOVEL     'COGE'        MOVSRR
293300911031      * SCADENZA
293400991018     C                   MOVE      MOVKCC        KKCC              4 0
293500991018     C                   MOVE      MOVKSC        KKSC              7 0
293600911031     C     KACO          CHAIN     CNACO00F                           22
293700911031     C                   MOVEA     ACOOPZ        OPZ
293800911031     C     OPZ(2)        IFEQ      '1'
293900911031     C                   Z-ADD     MOVDRG        MOVDSC
294000911031     C                   ELSE
294100911031     C                   Z-ADD     0             MOVDSC
294200911031     C                   END
294300920901     C* SE INC.ATTRIBUIRE DATA SCAD. = + 30 GG. DATA REGISTR.
294400020729     C     movkcc        IFEQ      §4OCC1
294500020729     C     movksc        ANDEQ     §4OSC1
294600930615     C*  SE DATA SCAD. =0 FORZO DT. REGISTR.
294700930615     C     MOVDSC        IFEQ      0
294800930615     C                   Z-ADD     MOVDRG        MOVDSC
294900930615     C                   END
295000941014     C                   MOVE      MOVDSC        G08INV
295100941014     C                   MOVE      *ZEROS        G08DAT
295200941014     C                   MOVEL     3             G08ERR
295300941014     C                   CALL      'XSRDA8'
295400941014     C                   PARM                    WLBDA8
295500941014     C     G08TGI        ADD       30            GI8TGI
295600941014     C                   MOVE      *ZEROS        GI8DAT
295700941014     C                   MOVE      *ZEROS        GI8INV
295800941014     C                   CALL      'XSRGI8'
295900941014     C                   PARM                    WGIDA8
296000941014     C                   MOVE      GI8INV        MOVDSC
296100920901     C                   END
296200991015      * CENTRO DI COSTO
296300991015     C     OPZ(3)        IFEQ      '1'
296400991021     C     WUNI          IFGT      *ZEROS
296500991022     C                   MOVEL     WUNI          MOVCDC
296600991021     C                   ELSE
296700991022     C                   MOVEL     §FLE          MOVCDC
296800991021     C                   END
296900991015     C                   ELSE
297000991015     C                   MOVE      *BLANKS       MOVCDC
297100991015     C                   END
297200991015     C                   Z-ADD     MOVDRG        MOVDCA
297300991015     C                   Z-ADD     MOVDRG        MOVDOP
297400911031      *
297500010622     C                   Z-ADD     1             MOVCMB
297600010622      *
297700991014     C                   WRITE     FNMOV000
297800911031      *
297900911031     C                   ENDSR
298000060213      *-----------------------------------------------------***********
298100060213      * SCRIVO MOVIMENTO CONTABILI                          *  WRTMOV *
298200060213      *-----------------------------------------------------***********
298300060213     C     WRTMOVp       BEGSR
298400060213     C*
298500060213     C* INCREMENTO NR. PRG REGISTRAZIONE
298600060213     C                   ADD       1             SUBRIG
298700060213     C                   Z-ADD     SUBRIG        MOVSBR
298800060213     C                   MOVE      KNMUS         MOVNMU
298900060213     C                   MOVE      KNRAZ         MOVRIF
299000060213     C                   MOVEL     §FLE          MOVRIF
299100060214     C                   Z-ADD     movnrg        MOVSBN
299200060213     C                   MOVEL     'T'           MOVFGE
299300060213     C                   MOVEL     '201'         MOVSOC
299400060213     C                   MOVEL     'D'           MOVTMO
299500060213     C                   MOVEL     'CG'          MOVCTB
299600060213     C                   Z-ADD     AMGUD8        MOVDRG
299700060213     C                   MOVEL     'COGE'        MOVSRR
299800060213     C                   MOVEL     §GEDMC        MOVDIP
299900060213     C                   MOVEL     §GEDMC        MOVDIv
300000060213      * SCADENZA
300100060213     C                   MOVE      MOVKCC        KKCC              4 0
300200060213     C                   MOVE      MOVKSC        KKSC              7 0
300300060213     C     KACO          CHAIN     CNACO00F                           22
300400060213     C                   MOVEA     ACOOPZ        OPZ
300500060213     C     OPZ(2)        IFEQ      '1'
300600060213     C                   Z-ADD     MOVDRG        MOVDSC
300700060213     C                   ELSE
300800060213     C                   Z-ADD     0             MOVDSC
300900060213     C                   END
301000060213      * CENTRO DI COSTO
301100060213     C     OPZ(3)        IFEQ      '1'
301200060213     C     WUNI          IFGT      *ZEROS
301300060213     C                   MOVEL     WUNI          MOVCDC
301400060213     C                   ELSE
301500060213     C                   MOVEL     §FLE          MOVCDC
301600060213     C                   END
301700060213     C                   ELSE
301800060213     C                   MOVE      *BLANKS       MOVCDC
301900060213     C                   END
302000060213     C                   Z-ADD     MOVDRG        MOVDCA
302100060214     C                   Z-ADD     MOVDRG        MOVDOP
302200060214     c* aggiungo x gg alla data registrazione inm genere 2
302300060214     c                   clear                   x17clvalds
302400060214     c                   eval      x17CalDtV = §4odom
302500060214     c                   eval      x17GgPiu = §4odog
302600060214     c                   move      movdrg        x17DatRif
302700060214     c                   call      'X17CLVAL'
302800060214     c                   parm                    x17clvalds
302900060214     c                   if        x17errore = 0
303000060214     c                   move      x17DatVal     movdop
303100060214     c                   end
303200060213      *
303300060213     C                   Z-ADD     1             MOVCMB
303400060213      *
303500060213     C                   WRITE     FNMOV000
303600060213      *
303700060213     C                   ENDSR
303800941014     C*-----------------------------------------------------
303900941014     C*  I N Z S R
304000941014     C*-----------------------------------------------------
304100941014     C     *INZSR        BEGSR
304200991014     C*
304300941014     C     *ENTRY        PLIST
304400941014     C                   PARM                    KPJBA
304500020919     c*
304600941014      *
304700991018     C                   MOVE      *ALL'-'       LINEA           152
304800941014     C                   Z-ADD     1             CODUT
304900941014     C                   CALL      'X§PARUT'
305000941014     C                   PARM                    UT§DSE
305100941014     C     CODUT         CABEQ     -1            FINE
305200020919     C*
305300020919     c     *dtaara       define    §azute        azuteds
305400020919     c     *dtaara       define    §datiute      ddatiute
305500020919     C                   in(E)     *dtaara
305600020919     C                   IF        %Error  or  RSUT = *blanks
305700020919     C                   call      'TIBS34R'
305800020919     C                   parm                    Tibs34Ds
305900020919     C                   in        *dtaara
306000020919     c                   ENDIF
306100050111     C*§
306200050111     c     dutlpo        ifeq      '2'
306300050111     c     dutlpo        oreq      *blanks
306400050111     c                   move      dutpou        §fle              3 0
306500050111     c                   else
306600050111     C     SIMFEL        COMP      0                                      02
306700050111     C  N02              MOVE      SIMFEL        §FLE
306800050111     C                   END
306900040921     C     dutpou        CHAIN     AZORG01L                           21
307000050111     c* se po contab. <> da po gestionale chiodo 2° altrimenti chiodo 1°
307100040921     c                   if        orgfc0 <> dutpou
307200040921     c                   eval      dutlpo = '2'
307300050111     c                   seton                                        01
307400050111     c                   else
307500050111     c                   setoff                                       01
307600050111     c                   eval      dutlpo = '1'
307700040921     c                   end
307800941014     C*§§
307900941014     C                   TIME                    WHHUDT           14 0
308000941014     C                   MOVEL     WHHUDT        ORA               6 0
308100941014     C                   MOVE      WHHUDT        G08DAT
308200941014     C                   MOVE      *ZEROS        G08INV
308300941014     C                   MOVEL     *BLANK        G08ERR
308400941014     C                   CALL      'XSRDA8'
308500991013     C                   PARM                    WLBDA8
308600941014     C                   Z-ADD     G08INV        AMGUD8            8 0
308700991018     C                   Z-ADD     G08DAT        GMAUD8            8 0
308800020919     C*
308900020919     C                   Z-ADD     §FLE          §§FGS
309000020919     C*
309100941014     C     §FLE          CHAIN     AZORG01L                           21
309200941014     C  N21              MOVEL     ORGDES        DFLE
309300941014     C*  CARICO I CONTI DA GENERARE  X FILIALE
309400020729     C                   MOVEL     'Y4O'         tbecod
309500020729     C                   MOVEl(p)  '201'         tbeke1
309600020729     C                   MOVEL(p)  §FLE          tbeke2
309700020729     C     ktbe1         CHAIN     Tntbe01l                           21
309800941014     C   21              SETON                                        88         MANCA TAB
309900020729     C  N21              MOVEL     TBeUNI        Dy4O
310000941014      * CARICO TUTTI I TIPI INCASSO  TAB.1A
310100941014     C                   MOVEL     '1A'          CODTAB
310200941014     C                   MOVE      *BLANKS       KEYTAB
310300941014     C                   MOVE      *ZEROS        X
310400941014     C     KTAB          SETLL     TABEL00F
310500941014     C     *IN21         DOUEQ     '1'
310600941014     C     KCOD          READE     TABEL00F                               21
310700941014     C  N21              ADD       1             X
310800941014     C  N21              MOVEL     TBLKEY        T1A(X)
310900941014     C  N21              MOVEL     TBLUNI        D1A(X)
311000941014     C                   END
311100941014      * CARICO TUTTI I TIPI BOLLA    TAB.3A
311200941014     C                   MOVEL     '3A'          CODTAB
311300941014     C                   MOVE      *BLANKS       KEYTAB
311400941014     C                   MOVE      *ZEROS        X
311500941014     C     KTAB          SETLL     TABEL00F
311600941014     C     *IN21         DOUEQ     '1'
311700941014     C     KCOD          READE     TABEL00F                               21
311800941014     C  N21              ADD       1             X
311900941014     C  N21              MOVEL     TBLKEY        T3A(X)
312000941014     C  N21              MOVEL     TBLUNI        D3A(X)
312100941014     C                   END
312200941014      *
312300941020      * CARICO TUTTI I CODICI VALUTA  TAB.CV
312400941020     C                   MOVEL     'CV'          CODTAB
312500941020     C                   MOVE      *BLANKS       KEYTAB
312600941020     C                   MOVE      *ZEROS        X
312700941020     C     KTAB          SETLL     TABEL00F
312800941020     C     *IN21         DOUEQ     '1'
312900941020     C     KCOD          READE     TABEL00F                               21
313000950123     C  N21TBLFLG        IFEQ      *BLANK
313100950123     C                   ADD       1             X
313200991018     C                   MOVEL     TBLKEY        CCV(X)
313300950123     C                   MOVEL     TBLUNI        DCV(X)
313400950123     C                   END
313500941020     C                   END
313600941020      *
313700941014     C* CERCO IL CODICE DELL'OPERATORE
313800941014     C*
313900020919     C                   MOVE      DUTCOU        VCDUS
314000941014      *  RICERCA CAPOCONTI
314100941014     C                   DO        50            X                 2 0
314200941014     C                   MOVE      TCU(X)        TCUDS
314300941014     C     F56           CABNE     'CG'          END1
314400941014     C     F4            COMP      '1'                                    21
314500941014     C     F3            COMP      '0'                                242425
314600941014      ** 1 CLIENTI   21
314700941014      ** 0 ITALIA   25
314800941014      ** 1 ESTERO   24
314900941014     C   21
315000941014     CAN 25              Z-ADD     KCU(X)        KCI               4 0
315100941014     C     END1          TAG
315200941014     C                   END
315300941014     C*
315400941014     C*------ APRO FILE SOTTO CONTROLLO SINCRONIA          *
315500941014     C*       SE NON RIESCO VADO A FINE
315600020919     C                   OPEN      FNARB71L                             10      *
315700991018     C  N10              OPEN      FNARB01L                             10      *
315800991018     C  N10              OPEN      FIARI01L                             10      *
315900060130     C  N10              OPEN      yfmdi01L                             10      *
316000941017     C  N10              OPEN      CNASS01L                             10      *
316100060131     C  N10              OPEN      fiinc02L                             10      *
316200941014     C*
316300941014     C                   OPEN      QSYSPRT
316400941014     C*
316500060131     C     *LIKE         DEFINE    incdmi        dmi
316600060131     C     *LIKE         DEFINE    incnmi        nmi
316700060131     C     *LIKE         DEFINE    TBLCOD        COD
316800941014     C     *LIKE         DEFINE    TBLKEY        KEY
316900941017     C     *LIKE         DEFINE    AR6IFT        §§IFT
317000941017     C     *LIKE         DEFINE    AR6IFT        §ABB
317100941014     C     *LIKE         DEFINE    ARBNMI        §NMI
317200941017     C     *LIKE         DEFINE    AR6DFT        §DFT
317300941017     C     *LIKE         DEFINE    AR6NFT        §NFT
317400991018     C     *LIKE         DEFINE    VCONEU        §ASNEU
317500991018     C     *LIKE         DEFINE    VCONEU        §PASEU
317600060214     C     *LIKE         DEFINE    VCONEU        §PASEUp
317700060214     C     *LIKE         DEFINE    VCONEU        §PASEUt
317800060131     C     *LIKE         DEFINE    VCONEU        §PreEU
317900060214     C     *LIKE         DEFINE    VCONEU        §PreEUp
318000060214     C     *LIKE         DEFINE    VCONEU        §PreEUt
318100060214     C     *LIKE         DEFINE    ctbcae        ctbcaet
318200941014     C     *LIKE         DEFINE    ARBKSC        §KSC
318300941014     C     *LIKE         DEFINE    ARBNMI        §§NMI
318400941017     C     *LIKE         DEFINE    AR6DFT        §SDFT
318500941017     C     *LIKE         DEFINE    AR6NFT        §SNFT
318600941014     C     *LIKE         DEFINE    ARBKSC        §SKSC
318700941017     C     *LIKE         DEFINE    AR9CAS        §ASS
318800991117     C     *LIKE         DEFINE    ARIIMP        §IPP
318900011115     C     *LIKE         DEFINE    §1afca        fatfca
319000011115     C     *LIKE         DEFINE    §1afca        casfca
319100060131     C     *LIKE         DEFINE    §1afca        prefca
319200020919     C     *LIKE         DEFINE    ARBIPP        §§FGS
319300941014      *---------------------------------------------------------------*
319400941014      * CHIAVI                                                        *
319500941014      *---------------------------------------------------------------*
319600941014     C     KTAB          KLIST
319700941014     C                   KFLD                    CODUT
319800941014     C                   KFLD                    CODTAB            2
319900941014     C                   KFLD                    KEYTAB            8
320000991011     C     KTBE          KLIST
320100020729     C                   KFLD                    TBECOD
320200020729     C                   KFLD                    TBEKE1
320300020729     C     KTBE1         KLIST
320400020729     C                   KFLD                    TBECOD
320500020729     C                   KFLD                    TBEKE1
320600020729     C                   KFLD                    TBEKE2
320700941014      *
320800941014     C     KCOD          KLIST
320900941014     C                   KFLD                    CODUT
321000941014     C                   KFLD                    CODTAB
321100941014      *
321200941014     C     KNMI          KLIST
321300020919     C                   KFLD                    §§FGS
321400020919     C                   KFLD                    §§NMI
321500941014     C                   KFLD                    VCDUS
321600941014     C                   MOVE      *ZEROS        §§NMI
321700941014      *
321800941014     C     KARB          KLIST
321900941014     C                   KFLD                    ARBAAS
322000941014     C                   KFLD                    ARBLNP
322100941014     C                   KFLD                    ARBNRS
322200991011     C                   KFLD                    ARBNSP
322300060131     C     Kspeblp       KLIST
322400060131     C                   KFLD                    incAAS
322500060131     C                   KFLD                    incLNP
322600060131     C                   KFLD                    incNRS
322700060131     C                   KFLD                    incNSP
322800991011     C     KAR6          KLIST
322900991011     C                   KFLD                    ARBAAS
323000991011     C                   KFLD                    ARBLNP
323100991011     C                   KFLD                    ARBNRS
323200941014     C                   KFLD                    ARBNSP
323300991011     C                   KFLD                    KTRC              1
323400060131     C     Kspear6       KLIST
323500060131     C                   KFLD                    incAAS
323600060131     C                   KFLD                    incLNP
323700060131     C                   KFLD                    incNRS
323800060131     C                   KFLD                    incNSP
323900060131     C                   KFLD                    inctrc            1
324000060131     c                   eval      inctrc = '1'
324100991011     C     KARI          KLIST
324200991011     C                   KFLD                    ARBAAS
324300991011     C                   KFLD                    ARBLNP
324400991011     C                   KFLD                    ARBNRS
324500991011     C                   KFLD                    ARBNSP
324600991019     C                   KFLD                    KTIP              1
324700991019     C     KARI1         KLIST
324800991019     C                   KFLD                    ARBAAS
324900991019     C                   KFLD                    ARBLNP
325000991019     C                   KFLD                    ARBNRS
325100991019     C                   KFLD                    ARBNSP
325200991019     C                   KFLD                    KTIP              1
325300991019     C                   KFLD                    KDIV              3
325400990311     C     KLBL          KLIST
325500990311     C                   KFLD                    LBLAAN
325600990311     C                   KFLD                    LBLLPN
325700990311     C                   KFLD                    LBLNRN
325800990311     C                   KFLD                    LBLNSN
325900941014      *
326000941014     C     KAAS          KLIST
326100941014     C                   KFLD                    §FLE
326200941014     C                   KFLD                    ARBLNP
326300941014     C                   KFLD                    ARBAAS
326400941014     C                   KFLD                    ARBNRS
326500941014     C                   KFLD                    ARBNSP
326600941014      *
326700060131     C     Kspeass       KLIST
326800060131     C                   KFLD                    §FLE
326900060131     C                   KFLD                    incLNP
327000060131     C                   KFLD                    incAAS
327100060131     C                   KFLD                    incNRS
327200060131     C                   KFLD                    incNSP
327300060330      *
327400060330     C     Kdpi          KLIST
327500060330     C                   KFLD                    knraz
327600060330     C                   KFLD                    comaasdpi
327700060330     C                   KFLD                    comlnpdpi
327800060330     C                   KFLD                    comNRSdpi
327900060330     C                   KFLD                    comNSPdpi
328000060131      *
328100060131     C     Kinc          KLIST
328200060131     C                   KFLD                    §FLE
328300060131     C                   KFLD                    vcdus
328400060131     C                   KFLD                    dmi
328500060131     C                   KFLD                    nmi
328600060131      *
328700941014     C     KACO          KLIST
328800941014     C                   KFLD                    CODUT
328900991018     C                   KFLD                    KKCC
329000991018     C                   KFLD                    KKSC
329100060328      *
329200060328     C     Kcas          KLIST
329300060328     C                   KFLD                    amgud8
329400060328     C                   KFLD                    §fle
329500021120      *
329600021120     C     Kqig          KLIST
329700021120     C                   KFLD                    qigann
329800021120     C                   KFLD                    §fle
329900021120     C                   KFLD                    amgud8
330000021120     c                   clear                   qigann
330100941014      *
330200991018     C* CARICO DATI TABELLA CORRENTE
330300991018     C                   MOVEL     'GED'         TBECOD
330400991018     C                   MOVEL     *BLANKS       TBEKE1
330500991018     C                   MOVEL     '1'           TBEKE1
330600991018     C     KTBE          CHAIN     TNTBE01L                           22
330700991018     C  N22              MOVEL     TBEUNI        DGED
330800021120     c* controllo che non sia già stata fatta la quadratura giornaliera
330900021120     c* se così fosse non lascio fare la chiusura operatore
331000060131     c     kqig          chain     yfqig01l
331100060131     c                   if        %found(yfqig01l)
331200021120     c                   seton                                        09
331300021120     c                   else
331400021120     c                   setoff                                       09
331500021120     c                   end
331600060330     c* crea il file FIDPI00F in qtemp x caricare gli incassi fatti con pos
331700060330     c                   call      'CNC0WC00C'
331800060330     c                   open      fidpi01l
331900991018     C*
332000941014     C                   ENDSR
332100900606      *----------------------------------------------------------------
332200941017     OFNARB000  E            AG0ARB
332300020919     O                       ARBIPP
332400020919     O                       ARBNMI
332500911031     O                       ARBDMI
332600020919     O                       ARBIFP
332700020919     O                       ARBNDC
332800941122     O                       ARBICA
332900990311     OFNARBMAM  E            AGGMAM
333000020919     O                       ARBIFP
333100990311     O                       ARBNDC
333200941014     OCNASS000  E            AGGASS
333300941014     O                       ASSFTR
333400941014     O                       ASSFVE
333500991019      *----------------------------------------------------------------
333600941021     OQSYSPRT   E            STANMI         1 02
333700911031     O                       RSUT              +  1
333800941021     O                       SIMFEL            +  1
333900911031     O                                         + 20 'CHIUSURA PER OPERATORE'
334000941021     O                       GMAUD8              90 '  /  /    '
334100941021     O                       §PGM              +  2
334200941021     O          E            STANMI         1
334300991013     O                       KNSIF             +  1
334400941021     O                       KNMUS             +  1
334500941021     O                       ORA                 90 '  :  :  '
334600911031     O          E            STANMI         1
334700961028     O                                         +  1 'PUNTO OPERATIVO DI.....:'
334800911031     O                       §FLE              +  1
334900911031     O                       DFLE              +  1
335000911031     O          E            STANMI         2
335100941021     O                                         +  1 'CODICE OPERATORE.......:'
335200911031     O                       VCDUS             +  1
335300911031     O                       KNMUS             +  1
335400991013     O          E            STANMI         2
335500991013     O                                         +  1 'NR. MANDATO DI INTROITO:'
335600020919     O                       §FLE              +  1
335700020919     O                                         +  0 '/'
335800020919     O                       §NMI          Z   +  0
335900991013     O                                         +  2 'DEL'
336000991013     O                       GMAUD8            +  1 '  /  /    '
336100991013     O          E            STANMI      1  2
336200991013     O                                           55 'IN EURO'
336300911031     O          E            STANMI         2
336400911031     O                                         +  1 'TOTALE CONTANTE .......:'
336500991018     O                       VCONEU        2     58
336600911031     O          E            STANMI         2
336700971125     O                                         +  1 'TOTALE NS.ASSEGNI......:'
336800991018     O                       VASNEU        2     58
336900060209     O          E            STANMI         2
337000060209     O                                         +  1 'TOTALE POS.............:'
337100060209     O                       Vpos          2     58
337200991013     O          E            STANMI         3
337300911031     O                                         +  1 'TOTALE DA INTROITARE...:'
337400991018     O                       VINTEU        2     58
337500991013     O          E            STANMI         1
337600991013     O                                         +  1 'TOTALE INCASSATO.......:'
337700991018     O                       VINCEU        2     58
337800060426     O                                         +  5 'DIFFERENZA.............:'
337900911105     O                       VABB          K   +  3
338000991013     O          E            STANMI         2
338100991019     O                                         +  1 '************************'
338200060127     O          E            STANMI      2  1
338300991013     O                                         + 58 'TOT. C/ASS. MITT. IN EUR'
338400991013     O                                         +  0 'O:'
338500991019     O                       VASMCE        K    105
338600110914     O                                          115 'SPEDIZ.:'
338700991013     O                       VNRMEU        Z   +  0
338800110914     O                                         +  1 'ASSEGNI:'
338900120125     O                       VNAMEU        Z   +  0
339000941017     O          E            STANMI         1
339100941019     O                                         + 58 'TOT. C/ASS. MITT. VALUTA'
339200941019     O                                         +  0 ' :'
339300991019     O                       VMIVAC        K    106
339400110914     O                                          115 'SPEDIZ.:'
339500941017     O                       VNRMIV        Z   +  0
339600110914     O                                         +  1 'ASSEGNI:'
339700110914     O                       VNAMIV        Z   +  0
339800911105     O          E            STANMI         2
339900941019     O                                         + 58 '************************'
340000911105     O                                         +  0 '**'
340100920311     O          E    80      STANMI         1
340200920311     O                                         +  1 'L'''
340300920311     O                                         +  1 'OPERATORE NON HA INCASSA'
340400920311     O                                         +  0 'TO ALCUNE SPEDIZIONI SEG'
340500920311     O                                         +  0 'NALANDO CHE SI DOVRA'''
340600920311     O                                         +  1 'RICHIEDERE NOTA ACCREDIT'
340700920311     O                                         +  0 'O.'
340800991118     O          E            STACTB      2  2
340900991118     O                                         +  1 'TOTALE P/ASSEGNATI.....:'
341000060214     O                       §PASEUt       2     58
341100991118     O          E            STACTB         2
341200991118     O                                         +  1 'TOTALE C/ASSEGNI ......:'
341300991118     O                       §ASNEU        2     58
341400060131     O          E            STACTB         2
341500060131     O                                         +  1 'TOTALE PREPAGATI ......:'
341600060214     O                       §preEUt       2     58
341700920623      *_________________________________________________________________________
341800920623      *
341900941021**         CMA
342000020920ALCOBJ OBJ((FNMOV00T *FILE *EXCL   M000000))
342100020920ALCOBJ OBJ((FNCAS00T *FILE *EXCL   M000000))
342200040407ALCOBJ OBJ((FNCAF00T *FILE *EXCL   M000000))
342300911031**         CMD1
342400020920ADDPFM FILE(FNMOV00T)               MBR(M000000)
342500020920ADDPFM FILE(FNCAS00T)               MBR(M000000)
342600911031**         CMD2
342700020920OVRDBF FILE(FNMOV00T)               MBR(M000000)
342800020920OVRDBF FILE(FNCAS00T)               MBR(M000000)
342900911031**         CMD3
343000020920CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000000)
343100020920CHKOBJ OBJ(FNCAS00T) OBJTYPE(*FILE) MBR(M000000)
