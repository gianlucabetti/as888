000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300030624     Ftrmz70d   CF   E             WORKSTN
000400030604     f                                     sfile(video2:snrr1)
000500080401     F                                     INFDS(DSFMT)
000600090317     Faitra01l  if   E           k disk
000700070723     F                                     RENAME(aitra000:aitra1)
000800070724     Faitra00f  uf a E             disk
000900090317     Faitrs01l  iF   E           k disk
001000030729     Ffiapd01l  iF   E           k disk
001100170221     FtnTBE01l  iF   E           k disk
001200011228      *
001300171025      *
001400090318      * definizione DS esterne interne
001500080401     D*-------------
001600080401     D DSFMT           DS           512
001700080401     D  $TASTO               369    369
001800080401     D  NRG                  370    370
001900080401     D  NCL                  371    371
002000080401     D  SF_RRN               376    377i 0
002100080401     D  NUM_RCDS             380    381i 0
002200080401     D*-------------
002300080401     D* posizione cursore
002400080401     D CURSOR          DS
002500080401     D  RIRI                   1      2B 0 INZ
002600080401     D  R$$                    2      2
002700080401     D  COCO                   3      4B 0 INZ
002800080401     D  C$$                    4      4
002900080401     D*-------------
003000020103     D Psds           SDS
003100020103     D  PgmName          *PROC
003200011228     D Kpjba         E DS
003300150618     d kpjbus                              like(kpjbu)
003400081201      *
003500030611     D tibs02ds      E DS
003501170904     D TIBS34DS      E DS                                                       *Profili utente
003502170904     D DDATIUTE      E DS                                                       *Dati utente
003503170904     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003504170904     D dute01        E DS                                                       *Utenti
003505170904     d dLat          e ds
003506170904     D TRUL31DS      E DS
003507170904     D TRUL31DS2     E DS
003600080109
004100081128     D fnlv24ds      E DS
004200090317     D trmz77ds      E DS
004300120103     D dtrsflr       E DS
004400150305AB   D dtrsflr1      E DS
004500090408     D trmz70s1ds    E DS
004600030604     D aitra         E DS                  extname(aitra00f)
004700170905     D*UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004800170905     D*CNCR80        E DS
005000170221     d dTPA          e ds
005100071113     d daip          e ds
005200081127     d datu          e ds
005400160208     d dsrg          e ds
005500150618     D dtrafiller    E DS
005600011227     d data            ds            10
005700011228     d aa                      3      4
005800011228     d mm                      6      7
005900011228     d gg                      9     10
006000150618     D*-------------
006100030613     D WLBDA8          DS
006200030613     D  G02DAT                 1      8  0
006300030613     D  G02INV                 9     16  0
006400030613     D  G02ERR                17     17
006500030613     D  G02TGI                18     22  0
006501170904      *
006502171025     d wabiv           s                   like(UteAut)
006503170904     d wabi            s                   like(UteAut)
006504170904     d wabiaut         s              1    inz
006505170904     d wabimez         s              1    inz
006506170904     d wabis           s                   like(UteAut)
006507170904     d wabisl          s              1    inz
006508170904     d* p.o. abilitati
006509171025     D $pognv          s              3  0 dim(250)
006510170904     D $pogn           s              3  0 dim(250)
006511170904     D $pog            s              3    dim(250)
006512170904     D $DIG            s              1    dim(20)
006513170904     D $ARG            s              3    dim(50)
006600080319      *
006700090316     D tibsSocI0...
006800090316     D               E DS                  QUALIFIED
006900090316     D                                     INZ
007000090316     D tibsSocO0...
007100090316     D               E DS                  QUALIFIED
007200090316     D                                     INZ
007300090316
007400171025      *
007500090316     ** Definizione variabili modulo/programma.
007600090316     **
007700090318     D rrnaitra        s             10i 0
007800090318     D modalita        S              1
007900090318     D x               S              4  0
008000090318     D WrkSqlCmd       S           1024
008100090318     D WrkEofS01       S              1
008200090318     D WrkCarS01       S              1
008300090318     D $VIDEO          S             10
008400090318     D snrr1           S              5i 0
008500090318     D SavOpzione      S                   like(v2csce)
008600090318     D Curr_page       S                   like(csrrrn)
008700090318     d dataoggi        s               d   datfmt(*iso)
008800090318     d datawday        s               d   datfmt(*iso)
008900090318     d dataiso         s               d   datfmt(*iso)
009000090318     d dataeur         s               d   datfmt(*eur)
009100090316     D prmRqsOpCode...
009200090316     D                 S             10A
009300090316     D prmRpyOpCode...
009400090316     D                 S             10A
009500090316     D prmRpyIdMsg...
009600090316     D                 S             10I 0
009700090316     D prmRqsFormato...
009800090316     D                 S             10A
009900090316     D prmRqsDataSize...
010000090316     D                 S             10I 0
010100090316     D prmRpyFormato...
010200090316     D                 S             10A
010300090316     D prmRpyDataSize...
010400090316     D                 S             10I 0
010500171025      *
010600011228      * Constants
010700011228      *     MaxKey - Maximum number of key fields allowed by this program
010800171025      *
010900030612     D MaxKey          C                   1
011000011228     D Descrizione     C                   '1'
011100011228     D Ascendente      C                   1
011200011228     D Carattere       C                   6
011300011228     D Put             C                   1
011400011228     D EndPut          C                   2
011500011228     D Get             C                   3
011600171025      *
011700011228      * Standalone fields
011800011228      *     SizeList   - Size of list
011900011228      *     ReturnSize - Size of list returned by sort API
012000171025      *
012100011228     D NotUsed         S             16A
012200011228     D ReturnSize      S              9B 0
012300011228     D SizeList        S              9B 0
012400011228     D RrnLast         S              5i 0
012500011228     D WrkSort         S              1
012600171025      *
012700011228      * Data Structures
012800011228      *     SflRcd     - The entire subfile record to pass to the sort
012900011228      *     QLGSCB     - The sort request block for the QLGSORT API
013000011228      *     QLGSCB00   - The sort request block for the QLGSRTIO API
013100011228      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
013200011228      *     QUSEC      - Error structure for the QLGSORT API
013300171025      *
013400011228     D SflRcd          DS
013500080401     D  v2csce
013600080401     D  v2trasoc
013700080401     D  v2traaut
013800080401     D  v2trafil
013900080401     D  v2trasocg
014000080401     D  v2tradfi
014001171025     D  v2abaut
014002171025     D  v2abmez
014100030604     D  Selected                      1A
014200011228
014300011228      /COPY QSYSINC/QRPGLESRC,QLGSORT
014400011228     D QLGKL                         16    DIM(MaxKey)
014500011228     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
014600011228     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
014700011228     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
014800011228     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
014900011228
015000011228      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
015100011228      /COPY QSYSINC/QRPGLESRC,QUSEC
015200090318      * ?___________________________________________________________________
015300171025      *
015400030604      *---------------- Gestione formato video video01----------------------
015500030604     C                   EXSR      Inzvideo1
015600030604     C                   DOU       $Video <> 'VIDEO1'
015700000608      *----------- Visualizzo dati di output in caso di errori.-------------
015800000608     C                   IF        *IN99
015900000608     C                   EVAL      *IN99 = *OFF
016000030604     C                   WRITE     video1
016100000608     C                   EVAL      *IN99 = *ON
016200000608     C                   ENDIF
016300000608      *
016400030604     C                   EXFMT     video1
016500000608      *
016600011227     C                   SELECT
016700030612     C                   WHEN      *INKC
016800011227     C                             OR
016900011227     C                             *INKL
017000011227     C                   EVAL      $Video = *BLANKS
017100011227
017200011228     C                   WHEN      *INKD
017300011228     C                   EXSR      Search
017400011227     C                   WHEN      *INKE
017500030604     C                   EXSR      Inzvideo1
017600011227     C                   OTHER
017700030604     C                   EXSR      Chkvideo1
017800080401     c                   Z-ADD     0             curr
017900080401     c                   Z-ADD     0             curc
018000090415     C                   Z-ADD     1             Curr_page
018100030604     C  N99              EXSR      Wrkvideo2c
018200011227     C                   ENDSL
018300011227      *
018400011227     C                   ENDDO
018500011227      *
018600011227     C                   EXSR      Uscita
018700090318      * ?___________________________________________________________________
018800011227     c* uscita
018900090318      * ?___________________________________________________________________
019000011227     C     Uscita        BEGSR
019100011227      *
019200090316     ** Chiudo il programma.x dati societari
019300090316     C                   CALL      'TIBSSOCR'
019400090316     C                   PARM      'FINALIZE'    prmRqsOpCode
019500090316     C                   PARM                    prmRpyOpCode
019600090316     C                   PARM                    prmRpyIdMsg
019700090316      *
019800011227     C                   EVAL      *INLR = *ON
019900011227     C                   RETURN
020000011227      *
020100011227     C                   ENDSR
020200090318      * ?___________________________________________________________________
020300011227     c* gestione sfl
020400090318      * ?___________________________________________________________________
020500030604     C     Wrkvideo2c    BEGSR
020600011227     c* compone comando sql
020700011227     c                   exsr      srsql
020800000627      *
020900011227     C                   EVAL      WrkCarS01 = *ON
021000030604     C                   EVAL      $Video = 'VIDEO2C'
021100011227      *
021200030604     C                   DOU       $Video <> 'VIDEO2C'
021300030604     C                   EXSR      Carvideo2
021400080401      *
021500080401     C                   WRITE     video2z
021600080402      *
021700080402     C                   EXSR      Posiziona_Curs
021800000627      *
021900030604     C                   EXFMT     video2c
021901171026     c                   setoff                                       28
021902171026     c                   clear                   $msg
022000080401     C*
022100080401     C* determino Riga/Colonna del cursore
022200080401     C                   MOVE      NRG           R$$
022300080401     C                   MOVE      NCL           C$$
022400080401     C                   Z-ADD     RIRI          CURR              2 0
022500080401     C                   Z-ADD     COCO          CURC              2 0
022600080401     C                   Z-ADD     CsrRRN        Curr_page
022700080401     C*
022800011227     C                   SELECT
022900011228     c* fine
023000011227     C                   WHEN      *INKC=*ON
023100011227     C                   EVAL      $VIDEO=*BLANKS
023200011228     c* guida
023300011227     C                   WHEN      *INKL=*ON
023400030604     C                   EVAL      $VIDEO='VIDEO1'
023500011228     c* ripristino
023600011227     C                   WHEN      *INKE=*ON
023700011227     C                   EVAL      WrkCarS01 = *ON
023800011228     c*Cambio ordinamento
023900011228     C                   WHEN      *INKK
024000011228     C                   EXSR      Sort
024100011228     c*ripeti opzione (NON ABILITATO)
024200011228     C                   WHEN      *INKM
024300011228     C                   EXSR      RipetiOpz
024400011227
024500011227     C                   OTHER
024600011227
024700090506     c                   if        csrrrn > 0
024800011227     C                   EXSR      GestioneSFL
024900090506     c                   endif
025000011227     C                   ENDSL
025100011228
025200011227     C                   ENDDO
025300011227     C                   ENDSR
025400090318      * ?___________________________________________________________________
025500080402     c* posiziona cursore alla riemissione del SFL
025600090318      * ?___________________________________________________________________
025700080402     C     Posiziona_CursBEGSR
025800080402      *
025900080402      * Controlla il cursore dove si trovava per l'ultima riga
026000080402      * selezionata con un'opzione
026100080402     c                   Z-ADD     CURR          H1RIGA
026200080402     c                   Z-ADD     CURC          H1COLO
026300080402      *
026400080402      *  Si vuole sempre posizionare sulla prima riga della pagina
026500080402      *    Salta i controlli qui sotto
026600080402     c                   Z-ADD     6             H1RIGA
026700080402     c                   goto      xOra_salta
026800080402      *
026900080402      * 5 righe di testata + il resto derivato dalle righe contenute
027000080402      *  nella pagina
027100080402     c                   z-add     5             Posiz_curs        3 0
027200080402     c     last_rig_opz  div       15            risulta           5 0
027300080402     c                   mvr                     resto             3 0
027400080402     c                   add       resto         Posiz_curs
027500080402      *
027600080402     c                   if        last_rig_opz > 0
027700080402     c                   if          last_rig_opz = num_rcds
027800080402     c                   Z-ADD     Posiz_curs    H1RIGA
027900080402     c                   else
028000080402      *
028100080402     c                   if           curr=6
028200080402      *
028300080402     c                   if           last_rig_opz  > num_rcds
028400080402     c                   Z-ADD     6             H1RIGA
028500080402     c                   else
028600080402     c                   if           last_rig_opz <> num_rcds - 1
028700080402     c                   Z-ADD     20            H1RIGA
028800080402     c                   end
028900080402     c                   end
029000080402      *
029100080402     c                   else
029200080402     c                   sub       1             H1RIGA
029300080402     c                   end
029400080402      *
029500080402     c                   end
029600080402     c                   end
029700080402      *
029800080402      *
029900080402     c     xOra_salta    tag
030000080402      *
030100080402      * se per qulache motivo sono diminuite le righe
030200080402      *  lo reimposta x non spaccare il DSPF
030300080402     c                   if        Curr_page > num_rcds
030400080402     C                   eval      Curr_page = num_rcds
030500080402     c                   goto      xOra_salta1
030600080402     c                   Z-ADD     Posiz_curs    H1RIGA
030700080402     c     xOra_salta1   tag
030800080402     c                   end
030900080402      *
031000080402      *  Quindi può riposizionarsi sulla pagina
031100080402     C                   eval      CsrRRN = Curr_page
031200080402     c                   if        Curr_page > 0
031300080402     C                   eval      Nrr1   = Curr_page
031400080402     c                   end
031500080402      *
031600080402     C                   ENDSR
031700090318      * ?___________________________________________________________________
031800011227     c* Controlli video R01.
031900090318      * ?___________________________________________________________________
032000030604     C     Chkvideo1     BEGSR
032100000609      *
032200000608     C                   EVAL      *IN99 = *OFF
032300080109     C                   EVAL      *IN81 = *OFF
032400080109     C                   EVAL      *IN82 = *OFF
032500090319     C                   EVAL      *IN85 = *OFF
032600081204     C                   EVAL      *IN88 = *OFF
032700011227      * raggruppamento
032800030604     C                   move      'C'           v1ctip
032900170904      * verifica filiale abilitata se immessa
032901171026     c                   if        wabi <> *blank and wabi <> 'NO'
033600171026     c                   if        v1cfil <> 0
033700171027     c*    v1cfil        lookup    $pogn                                  81
033800171027     c* n81              seton                                          9982
033900090605     c                   endif
033901171026     c                   endif
034500081204     c                   if        v1ccod <> 9999999 and v1ccod > *zeros
034600081204     c                             and v1taut = *blank
034700081204     c                   seton                                        8899
034800081204     c                   endif
034900090317      * se premuto F1 richiamo gestione società verifico se profilo autorizzato
035000090317     c                   if        *inka
035100170904     c                   if        wabisl = '2'
035200090326     c                   call      'TRMZ70SC'
035300090317     c                   parm                    kpjba
035400090409     c                   seton                                        99
035500090317     c                   else
035600090319     c                   seton                                        8599
035700090317     c                   endif
035800090317     c                   endif
035900090317      *
036000090323      * se premuto F2 richiamo gestione società x AREA
036100170914     c                   if        *inkb
036200170914     c                   if        wabisl = '2' or
036201170914     c                             wabisl = '3'
036300170914     c                   call      'TRMZ70SR5'
036400170914     c                   parm                    kpjba
036500090409     c                   seton                                        99
036600170914     c                   else
036700170914     c                   seton                                        8599
036800170914     c                   endif
036900170914     c                   endif
037000090326      * se premuto F7 richiamo ricerca per partita iva
037100090326     c                   if        *inkg
037200090326     c                   call      'TRMZ78R'
037300090326     c                   parm                    kpjba
037400090409     c                   seton                                        99
037500090326     c                   endif
037600091008      * se premuto F8 richiamo interrogazione società
037700091008     c                   if        *inkh
037800091008     c                   call      'TRMZ65SR'
037900091008     c                   parm                    kpjba
038000091008     c                   seton                                        99
038100091008     c                   endif
038200000608     C                   ENDSR
038300090318      * ?___________________________________________________________________
038400011227     c* inizializza r01
038500090318      * ?___________________________________________________________________
038600030604     C     Inzvideo1     BEGSR
038700000608
038800030617     C                   move      'N'           v1cdis
038900030604     C                   EVAL      $Video = 'VIDEO1'
039000030604     C                   EVAL      v1caut  = '*'
039100030604     C                   EVAL      v1csoc  = '*'
039200030604     C                   EVAL      v1csoca = '*'
039300030604     C                   EVAL      v1ctaa  = '*'
039400070523     C                   EVAL      v1csco  = '*'
039500070523     C                   EVAL      v1cris  = '*'
039600100113     C                   EVAL      v1ccor  = '*'
039700081202     C                   EVAL      v1ccod  = 9999999
039800030604     C                   EXSR      Chkvideo1
039900080328     C                   EVAL      dknmus  = knmus
040000080328     C                   EVAL      dknsif  = knsif
040100000609
040200000608     C                   ENDSR
040300090318      * ?___________________________________________________________________
040400011228     c* ricerca
040500090318      * ?___________________________________________________________________
040600011228     C     Search        BEGSR
040700011228     C                   ENDSR
040800090318      * ?___________________________________________________________________
040900030605     C     opzione       BEGSR
041000090318      * ?___________________________________________________________________
041100080401     c                   move      v2csce        modalita
041200080522      * se modalità diversa da copia o inserimento proteggo campo nome autista CHIAVE
041300030605     c                   select
041400170905     c                   when      v2csce = '2'
041500030605     c                   exsr      modifica
041600080401     c                   when      v2csce = '5'
041700030919     c                   setoff                                       559648
041800030606     c                   seton                                        21
041900030606     c                   exsr      modifica
042000030606     c                   setoff                                       21
042100030605     c                   endsl
042200030604     C                   ENDSR
042300090318      * ?___________________________________________________________________
042400030605     C     modifica      BEGSR
042500090318      * ?___________________________________________________________________
042600090318     c     v2nrrn        chain(n)  aitra00f
042700090318     c                   if        %found(aitra00f)
042800090316     c     tranrc        chain     aitrs01l
042900090318     c                   if        %found(aitrs01l)
043000080321      *
043100090316     ** Chiamo passando un id società che esiste.
043200090316     c                   clear                   vtrasocgd
043300090316     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
043400090316     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
043500090316     C                   RESET                   tibsSocI0
043600090316     C                   EVAL      tibsSocI0.idSocieta = trasocg
043700090316     C
043800090316     C                   CALL      'TIBSSOCR'
043900090316     C                   PARM      'GETANAGRAF'  prmRqsOpCode
044000090316     C                   PARM      *BLANK        prmRpyOpCode
044100090316     C                   PARM      *ZERO         prmRpyIdMsg
044200090316     C                   PARM      'TIBSSOCI0'   prmRqsFormato
044300090316     C                   PARM                    tibsSocI0
044400090316     C                   PARM                    prmRqsDataSize
044500090316     C                   PARM      'TIBSSOCO0'   prmRpyFormato
044600090316     C                   PARM                    tibsSocO0
044700090316     C                   PARM                    prmRpyDataSize
044800090324     c                   if         PRMRPYIDMSG >= 0 and
044900090324     c                              TIBSSOCO0.IDSOCIETA <> *blank
045000090324     c                   eval      vtrasocgd = tibsSocO0.RAGSOCIALE
045100090324     c                   else
045200090324     c                   eval      vtrasocgd = 'Società Errata'
045300090324     c                   end
045400120104     c                   setoff                                       19
045500120104     c                   eval      dtrsflr = trsflr
045600150305AB   c                   eval      dtrsflr1 = trsflr1
045700120821      **
045800120821      ***  pulisce il campo della 1°data certa
045900120821     c                   clear                   vtraunodc
046000120104     c                   if         §trsris = 'R'
046100120821     c*******            clear                   vtraunodc
046200120104     c                   setoN                                        19
046300120104     c                   end
046400150305      ******
046500150305AB    * Data DURC (è un campo della DS FLR1 ed è alfa)
046600150305     c                   if        §trsDDURC <> *blank and
046700150305     c                             §trsDDURC <> *zeros
046800150305     c                   move      §trsDDURC     data8             8 0
046900150305     c                   move      data8         dataiso
047000150305     c                   move      dataiso       dataeur
047100150305     c                   move      dataeur       vtrsDdurc
047200150305     c                   else
047300150305     c                   clear                   vtrsDdurc
047400150305     c                   endif
047500150305      **
047600150305AB    * Data  CIP (è un campo della DS FLR1 ed è alfa)
047700150305     c                   if        §trsDcip  <> *blank and
047800150305     c                             §trsDcip  <> *zeros
047900150305     c                   move      §trsDcip      data8
048000150305     c                   move      data8         dataiso
048100150305     c                   move      dataiso       dataeur
048200150305     c                   move      dataeur       vtrsDcip
048300150305     c                   else
048400150305     c                   clear                   vtrsDcip
048500150305     c                   endif
048600160208     c                   move      TRscor        vTRscor
048700160208     c                   if        vtrscor <> *blank
048800160208     c                   clear                   tibs02ds
048900160208     c                   movel     'C'           t02mod
049000160208     c                   movel     knsif         t02sif
049100160208     c                   movel     'SRG'         t02cod
049200160208     c                   movel     vtrscor       t02ke1
049300160208     c                   call      'TIBS02R'
049400160208     c                   parm                    KPJBA
049500160208     c                   parm                    TIBS02DS
049600160208     c                   if        t02err =  *blanks
049700160208     c                   movel     t02uni        dsrg
049800160208     c                   movel     §srgdes       vtrscord
049900160208     c                   endif
050000160208     c                   endif
050100150305      **
050200150305     c                   move      TRsnrc        vTRAnrc
050300150305     c                   move      TRsfil        vTRsfil
050400090409     c                   move      TRsSOCG       vTRASOCG
050500090318     c                   eval      vTRAiva = trsIVA
050600090318     c                   exsr      RagSocCont
050700090318     c                   eval      vragsoc = RagSocKsc
050800090317      *
050900090409     c                   if        trsdfc > 0
051000090409     c                   move      trsdfc        dataiso
051100090409     c                   move      dataiso       dataeur
051200090409     c                   move      dataeur       vtradfc
051300090409     c                   else
051400090409     c                   clear                   vtradfc
051500090409     c                   endif
051600120821      **
051700090409     c                   if        trsdsc > 0
051800090409     c                   move      trsdsc        dataiso
051900090409     c                   move      dataiso       dataeur
052000090409     c                   move      dataeur       vtradsc
052100120104     c  n19              if        §trsunodc = *zeros or
052200120104     c                             §trsunodc =  *blank
052300120104     c                   move      dataeur       vtraunodc
052400120104     c                   endif
052500090409     c                   else
052600090409     c                   clear                   vtradsc
052700090409     c                   endif
052800120821      **
052900120103     c                   if        §trsunodc <> *zeros and
053000120103     c                             §trsunodc <> *blank
053100120103     c                   move      §trsunodc     §trsunodc0        8 0
053200120103     c                   move      §trsunodc0    dataiso
053300120103     c                   move      dataiso       dataeur
053400120103     c                   move      dataeur       vtraunodc
053500120103     c                   endif
053600120103     c                   if        vtraunodc = vtradsc
053700120103     c                   clear                   vtradsc
053800120103     c                   endif
053900120103
054000090409     c                   if        trsdec > 0
054100090409     c                   move      TRsDEC        dataiso
054200090409     c                   move      dataiso       dataeur
054300130710     c                   move      dataeur       vTRsDec
054400090409     c                   else
054500130710     c                   clear                   vtrsdec
054600090409     c                   endif
054700090409     c                   if        trsdrc > 0
054800090409     c                   move      TRsDRC        dataiso
054900090409     c                   move      dataiso       dataeur
055000090409     c                   move      dataeur       vTRADrc
055100090409     c                   else
055200090409     c                   clear                   vtradrc
055300090409     c                   endif
055400120316     c                   if        trsdia > 0
055500120316     c                   move      TRsDia        dataiso
055600120316     c                   move      dataiso       dataeur
055700150331     c                   move      dataeur       vTRADsc2
055800120316     c                   else
055900150331     c                   clear                   vtradsc2
056000120316     c                   endif
056100090409      *fine trovato aitrs
056200090409     c                   endif
056300090409
056400090318     c                   move      trafil        vtrafil
056500030605     c                   move      TRATIP        vTRATIP
056600030605     c                   move      TRASOC        vTRASOC
056700030605     c                   move      TRAAUT        vTRAAUT
056701170904     c                   move      TRAcf         vTRAcf
056800030605     c                   move      TRARIS        vTRARIS
056900170221      ** decodifica tipologia autista
057000170221     c                   clear                   vtrarisD
057100170221     C                   movel     'TPA'         tbecod
057200170221     C                   movel(p)  traRIS        tbeke1
057300170221     C     Ktbe          chain     tntbe01l
057400170221     c                   if        %Found(tntbe01l)
057500170221     c                   movel     tbeUNI        DTPA
057600170221     c                   movel     §TPADES       vtrarisD
057700170221     c                   end
057800030729     c                   move      TRAtur        vTRAtur
057900030605     c                   move      TRATIA        vTRATIA
058000170626     c                   move      TRAatp        vTRAatp
058100070523     c                   move      TRASCO        vTRASCO
058200150324     c                   setoFF                                       58
058300150324      * protegge ITA/EST
058400150324     c                   if        vtraSCO <> ' '
058500150324     c                   setoN                                        58
058600150324     c                   end
058700030605     c                   move      TRATAA        vTRATAA
058800030618     c                   move      TRAcor        vTRAcor
058900030605     c                   move      TRAbpo        vTRAbpo
059000050526     c                   movel     tradass       vtradass
059100081127      * turnover
059200081127     c                   clear                   vtraturd
059300081127     c                   if        vtratur <> *blank
059400081127     c                   clear                   tibs02ds
059500081127     c                   movel     'C'           t02mod
059600081127     c                   movel     knsif         t02sif
059700081127     c                   movel     'ATU'         t02cod
059800081127     c                   movel     vtratur       t02ke1
059900081127     c                   call      'TIBS02R'
060000081127     c                   parm                    KPJBA
060100081127     c                   parm                    TIBS02DS
060200081127     c                   if        t02err =  *blanks
060300081127     c                   movel     t02uni        datu
060400081127     c                   movel     §atudes       vtraturd
060500081127     c                   endif
060600081127     c                   endif
060700030630      * decodifica blocca porte
060800030630     c                   clear                   vtrabpod
060900030630     c                   if        vtrabpo <> *blank
061000030630     c                   clear                   tibs02ds
061100030630     c                   movel     'C'           t02mod
061200030630     c                   movel     knsif         t02sif
061300030630     c                   movel     'ANT'         t02cod
061400030630     c                   movel     'VAN'         t02ke1
061500030630     c                   movel     vtrabpo       t02ke2
061600030630     c                   call      'TIBS02R'
061700030630     c                   parm                    KPJBA
061800030630     c                   parm                    TIBS02DS
061900030630     c                   if        t02err =  *blanks
062000030630     c                   movel     t02uni        vtrabpod
062100030630     c                   endif
062200030630     c                   endif
062300030605     c                   move      TRAANT        vTRAANT
062400160111     c                   movel     TRAper        vTRAper
062500160111     c                   move      TRAcen        vTRAcen
062600090929      *decodifica antifurto
062700090929     c                   clear                   vtraantd
062800090929     c                   if        vtraant <> *blank
062900090929     c                   clear                   tibs02ds
063000090929     c                   movel     'C'           t02mod
063100090929     c                   movel     knsif         t02sif
063200090929     c                   movel     'ANT'         t02cod
063300090929     c                   movel     'MOT'         t02ke1
063400090929     c                   movel     vtraant       t02ke2
063500090929     c                   call      'TIBS02R'
063600090929     c                   parm                    KPJBA
063700090929     c                   parm                    TIBS02DS
063800090929     c                   if        t02err =  *blanks
063900090929     c                   movel     t02uni        vtraantd
064000090929     c                   endif
064100090929     c                   endif
064200090929      *decodifica centrale operativa
064300090929     c                   clear                   vtracend
064400090929     c                   if        vtracen <> *blank
064500030630     c                   clear                   tibs02ds
064600030630     c                   movel     'C'           t02mod
064700030630     c                   movel     knsif         t02sif
064800090929     c                   movel     'CEN'         t02cod
064900090929     c                   movel     vtracen       t02ke1
065000090929     c                   clear                   t02ke2
065100030630     c                   call      'TIBS02R'
065200030630     c                   parm                    KPJBA
065300030630     c                   parm                    TIBS02DS
065400030630     c                   if        t02err =  *blanks
065500090929     c                   movel     t02uni        vtracend
065600030630     c                   endif
065700030630     c                   endif
065800030605     c                   move      TRATMP        vTRATMP
065900031002     c                   setoff                                       23
066000030606     c                   if        tradtm > 0
066100170905     c                   if        wabimez <> '2'
066200031002     c                   seton                                        23
066300031002     c                   end
066400040430     c                   move      TRADTM        dataiso
066500040430     c                   move      dataiso       dataeur
066600040430     c                   move      dataeur       vTRADTM
066700040430     c                   else
066800040430     c                   clear                   vtradtm
066900040430     c                   endif
067000031002
067100040430     c                   setoff                                       24
067200040430     c                   if        tradrr > 0
067201170905     c                   if        wabimez <> '2'
067400040430     c                   seton                                        24
067500040430     c                   end
067600040430     c                   move      TRADrr        dataiso
067700040430     c                   move      dataiso       dataeur
067800040430     c                   move      dataeur       vTRADrr
067900040430     c                   else
068000040430     c                   clear                   vtradrr
068100040430     c                   endif
068200040430
068300040430     c                   setoff                                       25
068400040430     c                   if        tradpr > 0
068401170905     c                   if        wabimez <> '2'
068600040430     c                   seton                                        25
068700040430     c                   end
068800040430     c                   move      TRADpr        dataiso
068900040430     c                   move      dataiso       dataeur
069000040430     c                   move      dataeur       vTRADpr
069100040430     c                   else
069200040430     c                   clear                   vtradpr
069300040430     c                   endif
069400040430
069500030606     c                   if        tradin > 0
069600030606     c                   move      TRADIN        dataiso
069700030606     c                   move      dataiso       dataeur
069800030606     c                   move      dataeur       vTRADin
069900030630     c                   else
070000030630     c                   clear                   vtradin
070100030606     c                   endif
070200030606     c                   if        tradfi > 0
070300030606     c                   move      TRADFI        dataiso
070400030606     c                   move      dataiso       dataeur
070500030606     c                   move      dataeur       vTRADfi
070600030630     c                   else
070700030630     c                   clear                   vtradfi
070800030606     c                   endif
070900030605     c                   move      TRAKAU        vTRAKAU
071000081126     C                   move      'A'           tipoa
071100081126     c                   z-add     vtrakau       vkcod
071200090528     c                   clear                   vtrakaud
071300090528     c     kapd          chain     fiapd01l
071400090528     c                   if        %found(fiapd01l)
071500090528     c                   movel     apdrsf        vtrakaud
071600090528     c                   if        apdatb <> *blank
071700090528     c                   move      ' Annullato'  vtrakaud
071800090528     c                   endif
071900090528     c                   endif
072000080320      *
072100081126     c                   move      TRABPT        vTRABPT
072200081126     C                   move      'D'           tipoa
072300081126     c                   z-add     vtrabpt       vkcod
072400090528     c                   clear                   vtrabptd
072500081126     c     kapd          chain     fiapd01l                           94
072600090528     c                   if        %found(fiapd01l)
072700090528     c                   movel     apdrsf        vtrabptd
072800090528     c                   if        apdatb <> *blank
072900090528     c                   move      ' Annullato'  vtrabptd
073000090528     c                   endif
073100090528     c                   endif
073200150619      *
073300030605     c                   move      TRAPUB        vTRAPUB
073400030605     c                   move      TRAFUR        vTRAFUR
073500070619     c                   move      TRArif        vTRArif
073600120914     c                   move      traloci       vtraloci
073700030630      *decodifica allestimento
073800030630     c                   clear                   vtrafurd
073900030630     c                   if        vtrafur <> *blank
074000030630     c                   clear                   tibs02ds
074100030630     c                   movel     'C'           t02mod
074200030630     c                   movel     knsif         t02sif
074300030630     c                   movel     'ALL'         t02cod
074400030630     c                   movel     vtrafur       t02ke1
074500030630     c                   call      'TIBS02R'
074600030630     c                   parm                    KPJBA
074700030630     c                   parm                    TIBS02DS
074800030630     c                   if        t02err =  *blanks
074900030630     c                   movel     t02uni        vtrafurd
075000030630     c                   endif
075100030630     c                   endif
075200060315      *decodifica classe omologazione
075300060315     c                   move      traff1        vtraff1
075400060315     c                   clear                   vtraff1d
075500060315     c                   if        vtraff1 <> *blank
075600060315     c                   clear                   tibs02ds
075700060315     c                   movel     'C'           t02mod
075800060315     c                   movel     knsif         t02sif
075900060315     c                   movel     'OMO'         t02cod
076000060315     c                   movel     vtraff1       t02ke1
076100060315     c                   call      'TIBS02R'
076200060315     c                   parm                    KPJBA
076300060315     c                   parm                    TIBS02DS
076400060315     c                   if        t02err =  *blanks
076500060315     c                   movel     t02uni        vtraff1d
076600060315     c                   endif
076700060315     c                   endif
076800060315      *decodifica tipo alimentazione
076900060315     c                   move      traff2        vtraff2
077000060315     c                   clear                   vtraff2d
077100060315     c                   if        vtraff2 <> *blank
077200060315     c                   clear                   tibs02ds
077300060315     c                   movel     'C'           t02mod
077400060315     c                   movel     knsif         t02sif
077500060315     c                   movel     'ALI'         t02cod
077600060315     c                   movel     vtraff2       t02ke1
077700060315     c                   call      'TIBS02R'
077800060315     c                   parm                    KPJBA
077900060315     c                   parm                    TIBS02DS
078000060315     c                   if        t02err =  *blanks
078100060315     c                   movel     t02uni        vtraff2d
078200060315     c                   endif
078300060315     c                   endif
078400030605     c                   move      TRASPI        vTRASPI
078500030605     c                   move      TRANT1        vTRANT1
078600030605     c                   move      TRANT2        vTRANT2
078700030606     c                   if        tradt2 > 0
078800030606     c                   move      TRADt2        dataiso
078900030606     c                   move      dataiso       dataeur
079000030606     c                   move      dataeur       vTRADt2
079100030630     c                   else
079200030630     c                   clear                   vtradt2
079300030606     c                   endif
079400030606     c                   if        tradp2 > 0
079500030606     c                   move      TRADP2        dataiso
079600030606     c                   move      dataiso       dataeur
079700030606     c                   move      dataeur       vTRADp2
079800030630     c                   else
079900030630     c                   clear                   vtradp2
080000030606     c                   endif
080100060710     c                   if        tradt3 > 0
080200060710     c                   move      TRADt3        dataiso
080300060710     c                   move      dataiso       dataeur
080400060710     c                   move      dataeur       vTRADt3
080500060710     c                   else
080600060710     c                   clear                   vtradt3
080700060710     c                   endif
080800060710     c                   if        tradt4 > 0
080900060710     c                   move      TRADt4        dataiso
081000060710     c                   move      dataiso       dataeur
081100060710     c                   move      dataeur       vTRADt4
081200060710     c                   else
081300060710     c                   clear                   vtradt4
081400060710     c                   endif
081500030605     c                   move      TRANRP        vTRANRP
081600160114      *  DISabilita il CMD(16) di Disaccr.Affl/Defl.
081700160114     c                   setoff                                       16
081800160114     c                   if        traBPT = 0 or traSCO <> *blank
081900160114     c                   setoN                                        16
082000160114     c                   end
082100030605     c                   exsr      Gesvideo3
082200030605     c                   endif
082300030605     C                   ENDSR
082400090318      * ?___________________________________________________________________
082500030605     C     copia         BEGSR
082600090318      * ?___________________________________________________________________
082700030605     C                   ENDSR
082800090318      * ?___________________________________________________________________
082900030605     C     gesvideo3     BEGSR
083000090318      * ?___________________________________________________________________
083100030605     c                   do        *hival
083101171030     c                   setoff                                       28
083200070524     c     sopra         tag
083201171025      * controllo livello abilitazione per modalità modifica
083202171025     c                   if        modalita = '2'
083203171025      *livello abilitazione per dati autista
083204171030     c                   if        v2abaut <> '2'
083205171030     c                   seton                                        20
083206171030     c                   endif
083207171025      *livello abilitazione per dati mezzo
083208171025     c                   if        v2abmez <> '2' and v2abmez <> '3'
083209171025     c                   seton                                        3021
083210171025     c                   endif
083211171025     c                   endif
083300030605     c                   exfmt     video3
083400090320     c*                  setoff                                       6358
083500120222      *
083600120222     c                   if        *inkl
083700120222     c                   setoff                                       28
083800120222     c                   leave
083900120222     c                   endif
084000150623      **
084100030919     c                   if        modalita <> '5'
084200030624     c                   exsr      controv3
084300030919     c                   end
084400030729     c   96              iter
084500080423      *
084600071009     c                   if        *inkf and not *in21
084700030729     c                   exsr      aggiorna
084800080423      *
084900030729     c                   leave
085000030729     c                   endif
085100160114      *
085200120222      *disaccreditamento parziale del solo codice di aff/def se doppio codice
085300120222      *e affluenza passata a trazionista
085400120222     c                   if        *inkq and modalita = '2'
085500120222     c                   exsr      disparziale
085600120222     c   96              iter
085700120222     c                   leave
085800120222     c                   endif
085900080423      *
086000080423      *  Se premuto F7 - dati automezzo
086100081201      *  altrimenti forzo l'emissione se profilo di filiale
086200170905     c                   if        not *inkg and wabiaut <> '2'
086300081201     c                   eval      *inkg = *on
086400081201     c                   endif
086500081201      *
086600030605     c                   if        *inkg
086700080423      *
086800090909     c                   if        vtraris <> *blank or vtracor <> *blank
086900090320     c*                  seton                                        63
087000070523     c                   goto      sopra
087100070523     c                   endif
087200080423      *
087300030605     c                   do        *hival
087400030606     c                   exfmt     video5
087500030605     c   kl              leave
087600030919     c                   if        modalita <> '5'
087700030624     c                   exsr      controv5
087800030919     c                   endif
087900030624     c   95              iter
088000080221     c                   if        *inkf and not *in21
088100030729     c                   exsr      aggiorna
088200030930     c                   goto      endges3
088300030729     c                   endif
088400030605     c                   enddo
088500030605     c                   endif
088600150401      *
088700030605     c                   enddo
088800030930     C     endges3       ENDSR
088900090316      * ?___________________________________________________________________
089000030624     C     Controv3      BEGSR
089100090316      * ?___________________________________________________________________
089200030613      *
089300030624      * *in96 errore generico videata autista
089400120222     c                   setoff                                       9628
089500080320      * -------------------
089600081127      *turnover
089700081127     c                   setoff                                       79
089800081127     c                   clear                   datu
089900090317     c                   clear                   vtraturd
090000081127     c                   if        vtratur = '?'
090100081127     c                   clear                   tibs02ds
090200081127     c                   movel     'R'           t02mod
090300081127     c                   movel     knsif         t02sif
090400081127     c                   movel     'ATU'         t02cod
090500081127     c                   call      'TIBS02R'
090600081127     c                   parm                    KPJBA
090700081127     c                   parm                    TIBS02DS
090800081127     c                   if        t02err =  *blanks
090900081127     c                   movel     t02uni        datu
091000081127     c                   movel     §AtuDES       vtraturd
091100081127     c                   movel     t02ke1        vtratur
091200081127     c                   goto      endctr3
091300081127     c                   else
091400081127     c                   seton                                        79
091500081127     c                   goto      endctr3
091600081127     c                   endif
091700081127     c                   else
091800081127     c                   if        vtratur <> *blank
091900081127     c                   clear                   tibs02ds
092000081127     c                   movel     'C'           t02mod
092100081127     c                   movel     knsif         t02sif
092200081127     c                   movel     'ATU'         t02cod
092300081127     c                   movel     vtratur       t02ke1
092400081127     c                   call      'TIBS02R'
092500081127     c                   parm                    KPJBA
092600081127     c                   parm                    TIBS02DS
092700081127     c                   if        t02err =  *blanks
092800081127     c                   movel     t02uni        datu
092900081127     c                   movel     §AtuDES       vtraturd
093000081127     c                   else
093100081127     c                   seton                                        79
093200081127     c                   goto      endctr3
093300081127     c                   endif
093400081127     c                   endif
093500081127     c                   endif
093600030613      *controlli data  fine rapporto
093700030613     c                   setoff                                       54
093800030613     c                   if        vtradfi > 0
093900030613     C                   MOVE      Vtradfi       G02DAT
094000030613     C                   MOVEL     *BLANK        G02ERR
094100030613     C                   CALL      'XSRDA8'
094200030613     C                   PARM                    WLBDA8
094300030613     C     G02ERR        IFEQ      '1'
094400130710     C                   SETON                                        5496
094500030729     c                   goto      endctr3
094600030613     C                   END
094700030613     c                   move      g02dat        vtradfi
094800160112      * controllo che data fine non sia minore di data accreditamento
094900160112     c                   if        g02inv < tradin
095000160112     C                   SETON                                        5496
095100160112     c                   goto      endctr3
095200160112     c                   endif
095300030613     c                   endif
095400150618      *
095500150619      *
095600030729     C     endctr3       ENDSR
095700090318      * ?___________________________________________________________________
095800030624     C     Controv5      BEGSR
095900090318      * ?___________________________________________________________________
096000030624      *
096100030624      * *in95 errore generico videata automezzo
096200090929     c                   setoff                                       95
096300030624      *targa  obbligatoria se gestione filiale
096400030624     c                   setoff                                       65
096500170906     c*m                 if        vtrataa = *blank and dutpou <> 046
096501170906     c*m                           and (wabimez = '2' or wabimez = '3')
096502170906     c                   if        vtrataa = *blank and wabimez = '3'
096600030624     c                   seton                                        6595
096700030729     c                   goto      endctr5
096800030624     c                   endif
096900050524      *data immatricolazione obbligatoria solo segnalazione
097000050524     c                   setoff                                       7273
097100170906     c*m                 if        vtradass = *zeros and dutpou <> 046
097101170906     c*m                           and (wabimez = '2' or wabimez = '3')
097102170906     c                   if        vtradass = *zeros and wabimez = '3'
097200050524     c                   seton                                        72
097300050524     c                   endif
097400050527     c                   if        vtradass > *zeros and vtradass < 1900
097500050527     c                   seton                                        73
097600050527     c                   goto      endctr5
097700050527     c                   endif
097800030624      *modello mezzo obbligatorio se gestione filiale
097900030624     c                   setoff                                       66
098000170906     c                   if        vtratia = *blank and wabimez = '3'
098100030624     c                   seton                                        6695
098200030729     c                   goto      endctr5
098300030624     c                   endif
098400030624      *allestimento mezzo obbligatorio se gestione filiale
098500030630     c                   setoff                                       6745
098600170906     c                   if        vtrafur = *blank and wabimez = '3'
098700030624     c                   seton                                        6795
098800030729     c                   goto      endctr5
098900030624     c                   endif
099000030624      *allestimento (tabella)
099100030624     c                   if        vtrafur = '?'
099200030624     c                   clear                   tibs02ds
099300030624     c                   movel     'R'           t02mod
099400030624     c                   movel     knsif         t02sif
099500030624     c                   movel     'ALL'         t02cod
099600030624     c                   call      'TIBS02R'
099700030624     c                   parm                    KPJBA
099800030624     c                   parm                    TIBS02DS
099900030624     c                   if        t02err =  *blanks
100000030624     c                   movel     t02uni        vtrafurd
100100030624     c                   movel     t02ke1        vtrafur
100200030624     c                   goto      endctr5
100300030624     c                   endif
100400030624     c                   else
100500030624     c                   if        vtrafur <> *blank
100600071114     c                   clear                   tibs02ds
100700030624     c                   movel     'C'           t02mod
100800030624     c                   movel     knsif         t02sif
100900030624     c                   movel     'ALL'         t02cod
101000030624     c                   movel     vtrafur       t02ke1
101100030624     c                   call      'TIBS02R'
101200030624     c                   parm                    KPJBA
101300030624     c                   parm                    TIBS02DS
101400030624     c                   if        t02err =  *blanks
101500030624     c                   movel     t02uni        vtrafurd
101600030624     c                   else
101700030624     c                   seton                                        4595
101800030729     c                   goto      endctr5
101900030624     c                   endif
102000030624     c                   endif
102100030624     c                   endif
102200030624      *sponda idraulica  obbligatoria se gestione filiale
102300030624     c                   setoff                                       68
102400170906     c                   if        vtraspi = *blank and wabimez = '3'
102500030624     c                   seton                                        6895
102600030729     c                   goto      endctr5
102700030624     c                   endif
102800030624      *colore  mezzo obbligatorio se gestione filiale
102900030624     c                   setoff                                       69
103000170906     c                   if        vtratmp = *blank and wabimez = '3'
103100030624     c                   seton                                        6995
103200030729     c                   goto      endctr5
103300030624     c                   endif
103400030624      *pubblicità mezzo obbligatoria se gestione filiale
103500030624     c                   setoff                                       70
103600170906     c                   if        vtrapub = *blank and wabimez = '3'
103700030624     c                   seton                                        7095
103800030729     c                   goto      endctr5
103900030624     c                   endif
104000090318      *rifrangente posteriore obbligatorio se gestione filiale
104100090318     c                   setoff                                       71
104200170906     c                   if        vtrarif = *blank and wabimez = '3'
104300090318     c                   seton                                        7195
104400090318     c                   goto      endctr5
104500090318     c                   endif
104600030624      * antifurto vano carico
104700030624     c                   setoff                                       46
104800081201      *antifurto vano carico obbligatorio se gestione filiale
104900170906     c                   if        vtrabpo = *blank and wabimez = '3'
105000081201     c                   seton                                        4695
105100081201     c                   goto      endctr5
105200081201     c                   endif
105300030624     c                   if        vtrabpo = '?  '
105400030624     c                   clear                   tibs02ds
105500030624     c                   movel     'R'           t02mod
105600030624     c                   movel     knsif         t02sif
105700030624     c                   movel     'ANT'         t02cod
105800030624     c                   movel(p)  'VAN'         t02ke1
105900030624     c                   call      'TIBS02R'
106000030624     c                   parm                    KPJBA
106100030624     c                   parm                    TIBS02DS
106200030624     c                   if        t02err =  *blanks
106300030624     c                   movel     t02uni        vtrabpod
106400030624     c                   movel     t02ke2        vtrabpo
106500030624     c                   endif
106600030624     c                   else
106700030624     c                   if        vtrabpo <> *blank
106800030624     c                   clear                   tibs02ds
106900030624     c                   movel     'C'           t02mod
107000030624     c                   movel     knsif         t02sif
107100030624     c                   movel     'ANT'         t02cod
107200030624     c                   movel     'VAN'         t02ke1
107300030624     c                   movel     vtrabpo       t02ke2
107400030624     c                   call      'TIBS02R'
107500030624     c                   parm                    KPJBA
107600030624     c                   parm                    TIBS02DS
107700030624     c                   if        t02err =  *blanks
107800030624     c                   movel     t02uni        vtrabpod
107900030624     c                   else
108000030624     c                   seton                                        4695
108100030729     c                   goto      endctr5
108200030624     c                   endif
108300030624     c                   endif
108400030624     c                   endif
108500080826      *
108600030624      * antifurto blocco motore
108700030624     c                   setoff                                       47
108800081201      *antifurto blocco motore obbligatorio se gestione filiale
108900170906     c                   if        vtraant = *blank and wabimez = '3'
109000081201     c                   seton                                        4795
109100081201     c                   goto      endctr5
109200081201     c                   endif
109300030624     c                   if        vtraant = '?  '
109400030624     c                   clear                   tibs02ds
109500030624     c                   movel     'R'           t02mod
109600030624     c                   movel     knsif         t02sif
109700030624     c                   movel     'ANT'         t02cod
109800030624     c                   movel(p)  'MOT'         t02ke1
109900030624     c                   call      'TIBS02R'
110000030624     c                   parm                    KPJBA
110100030624     c                   parm                    TIBS02DS
110200030624     c                   if        t02err =  *blanks
110300030624     c                   movel     t02uni        vtraantd
110400030624     c                   movel     t02ke2        vtraant
110500030624     c                   endif
110600030624     c                   else
110700030624     c                   if        vtraant <> *blank
110800030624     c                   clear                   tibs02ds
110900030624     c                   movel     'C'           t02mod
111000030624     c                   movel     knsif         t02sif
111100030624     c                   movel     'ANT'         t02cod
111200030624     c                   movel     'MOT'         t02ke1
111300030624     c                   movel     vtraant       t02ke2
111400030624     c                   call      'TIBS02R'
111500030624     c                   parm                    KPJBA
111600030624     c                   parm                    TIBS02DS
111700030624     c                   if        t02err =  *blanks
111800030624     c                   movel     t02uni        vtraantd
111900030624     c                   else
112000030624     c                   seton                                        4795
112100030729     c                   goto      endctr5
112200030624     c                   endif
112300030624     c                   endif
112400030624     c                   endif
112500090929      * centrali operative satellitari
112600090929     c                   setoff                                       41
112700090929     c                   if        vtracen = '?  '
112800090929     c                   clear                   tibs02ds
112900090929     c                   movel     'R'           t02mod
113000090929     c                   movel     knsif         t02sif
113100090929     c                   movel     'CEN'         t02cod
113200090929     c                   call      'TIBS02R'
113300090929     c                   parm                    KPJBA
113400090929     c                   parm                    TIBS02DS
113500090929     c                   if        t02err =  *blanks
113600090929     c                   movel     t02uni        vtracend
113700090929     c                   movel     t02ke1        vtracen
113800090929     c                   endif
113900090929     c                   else
114000090929     c                   if        vtracen <> *blank
114100090929     c                   clear                   tibs02ds
114200090929     c                   movel     'C'           t02mod
114300090929     c                   movel     knsif         t02sif
114400090929     c                   movel     'CEN'         t02cod
114500090929     c                   movel     vtracen       t02ke1
114600090929     c                   call      'TIBS02R'
114700090929     c                   parm                    KPJBA
114800090929     c                   parm                    TIBS02DS
114900090929     c                   if        t02err =  *blanks
115000090929     c                   movel     t02uni        vtracend
115100090929     c                   else
115200090929     c                   seton                                        4195
115300090929     c                   goto      endctr5
115400090929     c                   endif
115500090929     c                   endif
115600090929     c                   endif
115700060315      *Classe omologazione (tabella)
115800060315     c                   setoff                                       31
115900081201     c                   clear                   vtraff1d
116000081201      *Classe omologazione  obbligatorio se gestione filiale
116100170906     c                   if        vtraff1 = *blank and wabimez = '3'
116200081201     c                   seton                                        3195
116300081201     c                   goto      endctr5
116400081201     c                   endif
116500060315     c                   if        vtraff1 = '?'
116600060315     c                   clear                   tibs02ds
116700060315     c                   movel     'R'           t02mod
116800060315     c                   movel     knsif         t02sif
116900060315     c                   movel     'OMO'         t02cod
117000060315     c                   call      'TIBS02R'
117100060315     c                   parm                    KPJBA
117200060315     c                   parm                    TIBS02DS
117300060315     c                   if        t02err =  *blanks
117400060315     c                   movel     t02uni        vtraff1d
117500060315     c                   movel     t02ke1        vtraff1
117600060315     c                   goto      endctr5
117700060315     c                   endif
117800060315     c                   else
117900060315     c                   if        vtraff1 <> *blank
118000060315     c                   clear                   tibs02ds
118100060315     c                   movel     'C'           t02mod
118200060315     c                   movel     knsif         t02sif
118300060315     c                   movel     'OMO'         t02cod
118400060315     c                   movel     vtraff1       t02ke1
118500060315     c                   call      'TIBS02R'
118600060315     c                   parm                    KPJBA
118700060315     c                   parm                    TIBS02DS
118800060315     c                   if        t02err =  *blanks
118900060315     c                   movel     t02uni        vtraff1d
119000060315     c                   else
119100060315     c                   seton                                        3195
119200060315     c                   goto      endctr5
119300060315     c                   endif
119400060315     c                   endif
119500060315     c                   endif
119600060315      *tipo alimentazione (tabella)
119700060315     c                   setoff                                       32
119800090318      *tipo alimentazione  obbligatorio se gestione filiale
119900170906     c                   if        vtraff2 = *blank and wabimez = '3'
120000081201     c                   seton                                        3295
120100081201     c                   goto      endctr5
120200081201     c                   endif
120300070208     c                   clear                   vtraff2d
120400060315     c                   if        vtraff2 = '?'
120500060315     c                   clear                   tibs02ds
120600060315     c                   movel     'R'           t02mod
120700060315     c                   movel     knsif         t02sif
120800060315     c                   movel     'ALI'         t02cod
120900060315     c                   call      'TIBS02R'
121000060315     c                   parm                    KPJBA
121100060315     c                   parm                    TIBS02DS
121200060315     c                   if        t02err =  *blanks
121300060315     c                   movel     t02uni        vtraff2d
121400060315     c                   movel     t02ke1        vtraff2
121500060315     c                   goto      endctr5
121600060315     c                   endif
121700060315     c                   else
121800060315     c                   if        vtraff2 <> *blank
121900060315     c                   clear                   tibs02ds
122000060315     c                   movel     'C'           t02mod
122100060315     c                   movel     knsif         t02sif
122200060315     c                   movel     'ALI'         t02cod
122300060315     c                   movel     vtraff2       t02ke1
122400060315     c                   call      'TIBS02R'
122500060315     c                   parm                    KPJBA
122600060315     c                   parm                    TIBS02DS
122700060315     c                   if        t02err =  *blanks
122800060315     c                   movel     t02uni        vtraff2d
122900060315     c                   else
123000060315     c                   seton                                        3295
123100060315     c                   goto      endctr5
123200060315     c                   endif
123300060315     c                   endif
123400060315     c                   endif
123500030624      *controlli data  verniciatura
123600030624     c                   setoff                                       56
123700030624     c                   if        vtradtm > 0
123800030624     C                   MOVE      Vtradtm       G02DAT
123900030624     C                   MOVEL     *BLANK        G02ERR
124000030624     C                   CALL      'XSRDA8'
124100030624     C                   PARM                    WLBDA8
124200030624     C     G02ERR        IFEQ      '1'
124300030624     C                   SETON                                        5695
124400030729     c                   goto      endctr5
124500030624     C                   END
124600030624     c                   move      g02dat        vtradtm
124700030624     c                   endif
124800030624      *controlli data  verniciatura 2
124900030624     c                   setoff                                       57
125000030624     c                   if        vtradt2 > 0
125100030624     C                   MOVE      Vtradt2       G02DAT
125200030624     C                   MOVEL     *BLANK        G02ERR
125300030624     C                   CALL      'XSRDA8'
125400030624     C                   PARM                    WLBDA8
125500030624     C     G02ERR        IFEQ      '1'
125600030624     C                   SETON                                        5795
125700030729     c                   goto      endctr5
125800030624     C                   END
125900030624     c                   move      g02dat        vtradt2
126000030624     c                   endif
126100030624      *controlli data  ispezione mezzo
126200040610     c                   setoff                                       6064
126300030624     c                   if        vtradrr > 0
126400030624     C                   MOVE      Vtradrr       G02DAT
126500030624     C                   MOVEL     *BLANK        G02ERR
126600030624     C                   CALL      'XSRDA8'
126700030624     C                   PARM                    WLBDA8
126800030624     C     G02ERR        IFEQ      '1'
126900030624     C                   SETON                                        6095
127000030729     c                   goto      endctr5
127100030624     C                   END
127200040610      *- Nuovo controllo la data immessa deve essere quella del giorno e non altro
127300040610     C                   move      G02INV        datawday
127400040628     C                   IF        datawday <> dataoggi  and
127500040628     c                             *in21 = *off and *in24 = *off
127600170907     c                   if        dutpou <> 046
127700170907     C                   SETON                                        6495
127800170907     c                   goto      endctr5
127900170907     C                   END
128000040611     C                   END
128100040610      *-
128200030624     c                   move      g02dat        vtradrr
128300030624     c                   endif
128400030624      *controlli data  previsto ripristino
128500090320     c                   setoff                                       6163
128600030624     c                   if        vtradpr > 0
128700090320      * non si può inserire la data ripristino se la data ispezione è a 0
128800090320     c                   if        vtradrr = 0
128900090320     C                   seton                                        6395
129000090320     c                   goto      endctr5
129100090320     C                   END
129200030624     C                   MOVE      Vtradpr       G02DAT
129300030624     C                   MOVEL     *BLANK        G02ERR
129400030624     C                   CALL      'XSRDA8'
129500030624     C                   PARM                    WLBDA8
129600030624     C     G02ERR        IFEQ      '1'
129700090320     C                   SETON                                        6195
129800030729     c                   goto      endctr5
129900030624     C                   END
130000030624     c                   move      g02dat        vtradpr
130100030624     c                   endif
130200030624      *controlli data  ripristino 2
130300090320     c                   setoff                                       6277
130400030624     c                   if        vtradp2 > 0
130500090320     c                   if        vtradrr = 0
130600090320     C                   SETON                                        7795
130700090320     c                   goto      endctr5
130800090320     C                   END
130900030624     C                   MOVE      Vtradp2       G02DAT
131000030624     C                   MOVEL     *BLANK        G02ERR
131100030624     C                   CALL      'XSRDA8'
131200030624     C                   PARM                    WLBDA8
131300030624     C     G02ERR        IFEQ      '1'
131400030624     C                   SETON                                        6295
131500030729     c                   goto      endctr5
131600030624     C                   END
131700030624     c                   move      g02dat        vtradp2
131800030624     c                   endif
131900060619      *controlli data  scadenza RCV
132000060619     c                   setoff                                       76
132100060619     c                   if        vtradt3 > 0
132200060619     C                   MOVE      Vtradt3       G02DAT
132300060619     C                   MOVEL     *BLANK        G02ERR
132400060619     C                   CALL      'XSRDA8'
132500060619     C                   PARM                    WLBDA8
132600060619     C     G02ERR        IFEQ      '1'
132700060619     C                   SETON                                        7695
132800060619     c                   goto      endctr5
132900060619     C                   END
133000060619     c                   move      g02dat        vtradt3
133100060619     c                   endif
133200060619      *controlli data  disdetta RCV
133300060619     c                   setoff                                       75
133400060619     c                   if        vtradt4 > 0
133500060619     C                   MOVE      Vtradt4       G02DAT
133600060619     C                   MOVEL     *BLANK        G02ERR
133700060619     C                   CALL      'XSRDA8'
133800060619     C                   PARM                    WLBDA8
133900060619     C     G02ERR        IFEQ      '1'
134000060619     C                   SETON                                        7595
134100060619     c                   goto      endctr5
134200060619     C                   END
134300060619     c                   move      g02dat        vtradt4
134400060619     c                   endif
134500030624     C     endctr5       ENDSR
134600120222      * ?___________________________________________________________________
134700120222     C     disparziale   BEGSR
134800120222      * ?___________________________________________________________________
134900120222     c                   if        vtrabpt = 0
135000120222     c                   eval      $msg = 'Comando non eseguibile codice -
135100120222     c                             AF/DE non valorizzato'
135200120222     c                   seton                                        2896
135300120222     c                   leavesr
135400120222     c                   endif
135500120222     c                   if        vtrabpt > 0 and vtrakau = 0
135600120222     c                   eval      $msg = 'Comando non eseguibile codice -
135700120222     c                             città non valorizzato disaccreditare -
135800120222     c                             definitivamente'
135900120222     c                   seton                                        2896
136000120222     c                   leavesr
136100120222     c                   endif
136200120222     c                   if        vtradfi = 0
136300120222     c                   eval      $msg = 'Comando non eseguibile data -
136400120222     c                             disaccreditamento non impostata'
136500120222     c                   seton                                        2896
136600120222     c                   leavesr
136700120222     c                   else
136800120222     C                   MOVE      Vtradfi       G02DAT
136900120222     C                   MOVEL     *BLANK        G02ERR
137000120222     C                   CALL      'XSRDA8'
137100120222     C                   PARM                    WLBDA8
137200120222     C     G02ERR        IFEQ      '1'
137300120222     C                   SETON                                        5495
137400120222     c                   leavesr
137500120222     C                   END
137600120222     c                   endif
137700120222      *aggiorna record in linea eliminando codice aff/def
137800120222     c     v2nrrn        chain     aitra00f
137900120222     c                   if        %found(aitra00f)
138000120222      * gestione data tecnoblock
138100120222     c                   move      *zeros        trabpt
138200120222     c                   update    aitra000
138300120222      *scrive record disaccreditato con codice aff/def
138400120222     c                   move      *zeros        trakau
138500120222     c                   move      vtrabpt       trabpt
138600120222     c                   move      '8'           tratur
138700120222     c                   move      g02inv        tradfi
138800120222     c                   write     aitra000
138900120222     c                   endif
139000120222      *
139100120222     C                   ENDSR
139200090318      * ?___________________________________________________________________
139300030605     C     aggiorna      BEGSR
139400090318      * ?___________________________________________________________________
139500070524      * campi video3
139600090316     c                   if        modalita = '2'
139700090318     c     v2nrrn        chain     aitra00f
139800090316     c                   if        %found(aitra00f)
139900070524     c                   if        vtradfi > 0
140000070524     c                   move      vTRADFI       dataeur
140100070524     c                   move      dataeur       dataiso
140200070524     c                   move      dataiso       TRADfi
140300070524     c                   endif
140400070528      * se impostato autista di riserva abblenca dati automezzo
140500090909     c                   if        vtraris <> *blank  or vtracor <> *blank
140600070528     c                   clear                   video5
140700091014     c                   if        tratmp <> *blank
140800070528     c                   move      'S'           vtratmp
140900091014     c                   endif
141000070528     c                   endif
141100090417     c                   move      vTRAtur       TRAtur
141200070524      * campi video5
141300030610     c                   move      vTRAbpo       TRAbpo
141400030610     c                   move      vTRAANT       TRAANT
141500160111     c                   move      vTRAcen       TRAcen
141600160111     c                   movel     vTRAper       TRAper
141700030610     c                   move      vTRATMP       TRATMP
141800070524     c                   move      vTRATIA       TRATIA
141900070524     c                   move      vTRATAA       TRATAA
142000050524     c                   if        vtradass > 0
142100050526     c                   movel     vTRAdass      tradass
142200050526     c                   move      0101          tradass
142300050524     c                   else
142400050524     c                   clear                   tradass
142500050524     c                   endif
142600030610     c                   if        vtradtm > 0
142700030610     c                   move      vTRADTM       dataeur
142800030610     c                   move      dataeur       dataiso
142900030610     c                   move      dataiso       TRADTM
143000031003     c                   else
143100031003     c                   clear                   tradtm
143200030610     c                   endif
143300030610     c                   move      vTRAPUB       TRAPUB
143400030610     c                   move      vTRAFUR       TRAFUR
143500070619     c                   move      vTRArif       TRArif
143600030610     c                   move      vTRASPI       TRASPI
143700030610     c                   move      vTRANT1       TRANT1
143800030610     c                   move      vTRANT2       TRANT2
143900060315     c                   move      vTRAff1       TRAff1
144000060315     c                   move      vTRAff2       TRAff2
144100120914     c                   move      vTRAloci      TRAloci
144200150309      *
144300030610     c                   if        vtradt2 > 0
144400030610     c                   move      vTRADt2       dataeur
144500030610     c                   move      dataeur       dataiso
144600030610     c                   move      dataiso       TRADt2
144700031003     c                   else
144800031003     c                   clear                   tradt2
144900030610     c                   endif
145000030610     c                   if        vtradrr > 0
145100030610     c                   move      vTRADRR       dataeur
145200030610     c                   move      dataeur       dataiso
145300030610     c                   move      dataiso       TRADrr
145400031003     c                   else
145500031003     c                   clear                   tradrr
145600030610     c                   endif
145700030610     c                   if        vtradpr > 0
145800030610     c                   move      vTRADPR       dataeur
145900030610     c                   move      dataeur       dataiso
146000030610     c                   move      dataiso       TRADpr
146100031003     c                   else
146200031003     c                   clear                   tradpr
146300030610     c                   endif
146400030610     c                   if        vtradp2 > 0
146500030610     c                   move      vTRADP2       dataeur
146600030610     c                   move      dataeur       dataiso
146700030610     c                   move      dataiso       TRADp2
146800031003     c                   else
146900031003     c                   clear                   tradp2
147000030610     c                   endif
147100060619     c                   if        vtradt3 > 0
147200060619     c                   move      vTRADt3       dataeur
147300060619     c                   move      dataeur       dataiso
147400060619     c                   move      dataiso       TRADt3
147500060619     c                   else
147600060619     c                   clear                   tradt3
147700060619     c                   endif
147800060619     c                   if        vtradt4 > 0
147900060619     c                   move      vTRADt4       dataeur
148000060619     c                   move      dataeur       dataiso
148100060619     c                   move      dataiso       TRADt4
148200060619     c                   else
148300060619     c                   clear                   tradt4
148400060619     c                   endif
148500030610     c                   move      vTRANRP       TRANRP
148600150616     c                   eval      tradp4    = wdatg
148700150616      **
148800030610     c                   update    aitra000
148900080901     c                   endif
149000030610     c                   endif
149100030605     C                   ENDSR
149200090318      * ?___________________________________________________________________
149300011228     c* carica sfl
149400090318      * ?___________________________________________________________________
149500030604     C     Carvideo2     BEGSR
149600011228     C                   IF        WrkCarS01 = *ON
149700030604     C                   EVAL      snrr1   = 0
149800011228     C                   EVAL      *IN90 = *ON
149900020219     C                   EVAL      *IN91 = *OFF
150000030604     C                   WRITE     video2c
150100011228     C                   EVAL      *IN90 = *OFF
150200080328     C                   EVAL      dknmus  = knmus
150300080328     C                   EVAL      dknsif  = knsif
150400030604     c* Lettura aitra01l
150500011228     C/EXEC SQL
150600011228     C+ PREPARE S1 FROM :WrkSqlCmd
150700011228     C/END-EXEC
150800011228
150900011228     C/EXEC SQL
151000011228     C+ DECLARE A1 CURSOR FOR S1
151100011228     C/END-EXEC
151200011228
151300011228     C/EXEC SQL
151400011228     C+ OPEN A1
151500011228     C/END-EXEC
151600011228
151700011228     C                   DOU       SqlCod <> 0
151800011228     C/EXEC SQL
151900090317     C+ FETCH NEXT FROM A1 INTO :rrnaitra, :aitra
152000011228     C/END-EXEC
152100011228     c*
152200011228     C                   SELECT
152300011228     C                   WHEN      SqlCod = 0
152301170904      * se non esplicitata filiale da estrarre carico solo le filiali abilitate
152302170904      * al profilo
152303170904     c                   if        v1cfil = 0
152304170904     c     trafil        lookup    $pogn                                  81
152305171025     c                   if        *in81
152309171026     c                   move      wabiaut       v2abaut
152310171025     c                   move      wabimez       v2abmez
152311171025     c                   else
152312171026     c                   clear                   v2abaut
152313171026     c                   clear                   v2abmez
152314170904     c                   endif
152315171030     c                   else
152316171030     c                   move      wabiaut       v2abaut
152317171030     c                   move      wabimez       v2abmez
152318171026     c                   endif
152319171025      *se l'abilitazione alla visualizzazione è diversa da azienda filtro per le abilitate
152320171025     c                   if        wabiv <> 'AZ'
152321171025     c     trafil        lookup    $pognv                                 81
152322171025     c  n81              iter
152323171025     c                   endif
152324170904      *
152400080401     c                   movel     *blanks       v2csce
152500090317     c                   movel     trasoc        v2trasoc
152600090317     c                   movel     traaut        v2traaut
152700090317     c                   movel     trafil        v2trafil
152800090317     c                   movel     trasocg       v2trasocg
152900080401     c                   clear                   v2tradfi
153000090317     c                   z-add     rrnaitra      v2nrrn
153100030729     c                   if        tradfi > 0
153200030729     c                   move      tradfi        dataiso
153300030729     c                   move      dataiso       dataeur
153400080401     c                   move      dataeur       v2tradfi
153500030729     c                   endif
153600030617      *
153700030617      *esco per raggiunta massima capacità sfl
153800030617     c                   if        snrr1 > 9990
153900030617     c                   leave
154000030617     c                   endif
154100030617
154200030604     C                   EVAL      snrr1 = snrr1 + 1
154300030604     C                   WRITE     video2
154400011228     C                   WHEN      SqlCod = 100
154500011228     C                   WHEN      SqlCod <> 0
154600011228      * Forzo la stampa del JOBLOG.
154700011228     C                   CALL      'X66CHGJOB'
154800011228     C                   ENDSL
154900011228     C                   ENDDO
155000011228     C/EXEC SQL
155100011228     C+ CLOSE A1
155200011228     C/END-EXEC
155300030604     C                   IF        snrr1  > 0
155400020219     C                   EVAL      *IN91 = *ON
155500020219     C                   MOVE      1             OK                1 0
155600020219     C                   ELSE
155700020219     C                   MOVE      0             OK                1 0
155800020219     C                   ENDIF
155900071119     c*
156000030604     C                   EVAL      RrnLast = snrr1
156100011228     C                   EVAL      WrkSort = ' '
156200030612     c*                  exsr      sort
156300030612     C                   EVAL      nrr1  = 1
156400011228     C                   ENDIF
156500080401     C*****              EVAL      WrkCarS01 = *OFF
156600030604     C                   EVAL      snrr1  = 1
156700030604     C                   EVAL      csrrrn = 1
156800011228     C                   ENDSR
156900090318      * ?___________________________________________________________________
157000011227     c* gestione sfl
157100090318      * ?___________________________________________________________________
157200000613     C     GestioneSFL   BEGSR
157300000627      *
157400000627     C                   EVAL      WrkEofS01 = *OFF
157500030605      * Elaborazione opzioni.
157600011228     c*
157700080402     c                   z-add     0             last_rig_opz      4 0
157800030605     c                   do        *hival        X                 4 0
157900030605     c     X             CHAIN     video2                             50
158000030605     C   50              LEAVE
158100080513     c                   clear                   modalita
158200080522     c                   setoff                                       38
158300080401     C                   IF        v2csce <> *blanks
158301171025     c                   if        v2csce = '2'  and
158302171025     c                             v2abaut = ' ' and v2abmez = ' '
158303171025     c                   seton                                        28
158304171026     c                   eval      $msg = 'Opzione non ammessa -
158305171026     c                             anagrafica selezionata non in gestione'
158306171025     c                   else
158400030605     c                   exsr      opzione
158401171025     c                   endif
158500080402     c                   z-add     x             last_rig_opz
158600080401     c                   move      *blank        v2csce
158700030605     c                   update    video2
158800030611     c                   setoff                                       22
158900030611
159000011228     C                   ENDIF
159100030605     C                   enddo
159200000627      *
159300000613     C                   ENDSR
159400090318      * ?___________________________________________________________________
159500090318     C     RagSocCont    BegSR
159600090318      * ?___________________________________________________________________
159700090318      *
159800090408     C                   clear                   trmz70s1ds
159900090320     C                   movel(p)  TRSiva        PartitaIVA                     Input
160000090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
160100090320     C                   movel(p)  TRSSOCG       Societa                        Input/Output
160200090320     C                   movel(p)  TRSFIL        Unita                          Input/Output
160300090318      *
160400090320     c                   call      'TRMZ70SR1'
160500090408     C                   parm                    trmz70s1ds
160600090318     c*
160700090320     C                   ENDSR
160800090318      * ?___________________________________________________________________
160900000703      * Ripetizione opzione in tutte le righe del subfile.
161000090318      * ?___________________________________________________________________
161100000703     C     RipetiOpz     BEGSR
161200000703      *
161300030604     C                   IF        CsrRrn > *ZERO
161400030604     C                   EVAL      Snrr1 = CsrRrn
161500030604     C     snrr1         CHAIN     video2
161600080401     C                   IF        %FOUND and v2csce <> *blanks
161700080401     C                   EVAL      SavOpzione = v2csce
161800030604     C                   UPDATE    video2
161900000703     C                   EVAL      WrkEofS01 = *OFF
162000000703     C                   DOU       WrkEofS01 = *ON
162100030604     C                   ADD       1             snrr1
162200030604     C     snrr1         CHAIN     video2
162300000703     C                   IF        %FOUND
162400080401     C                   EVAL      v2csce = SavOpzione
162500030604     C                   UPDATE    video2
162600000703     C                   ELSE
162700000703     C                   EVAL      WrkEofS01 = *ON
162800000703     C                   ENDIF
162900000703     C                   ENDDO
163000000703     C                   ENDIF
163100000703     C                   ENDIF
163200000703      *
163300000703     C                   ENDSR
163400090318      * ?___________________________________________________________________
163500011228      * Subroutine - Sort
163600011228      *   This subroutine sorts the subfile records.
163700090318      * ?___________________________________________________________________
163800011228     C     Sort          BEGSR
163900080401     C                   CLEAR                   v2traaut
164000080401     C                   CLEAR                   v2trasoc
164100030604     C                   EVAL      nrr1  = 1
164200011228
164300011228      * Initialize the key fields to sort on. There is one extra field not in
164400011228      * the subfile -- Selected -- that is added to the record. This field
164500011228      * is used to place selected records in front of nonselected records.
164600011228
164700011228     C                   CLEAR                   QLGSCB
164800011228     C                   CLEAR                   QLGSCB00
164900011228
165000011228     ** Gestione della scelta ordinamento.
165100011228     C                   SELECT
165200011228     ** Ordinamento per descrizione.
165300011228     C                   WHEN      WrkSort = Descrizione
165400011228     ** 1 campo chiave.
165500011228     C                   EVAL      QLGNBRK    = 1
165600011228     ** La descrizione è
165700030612     ** a posizone 5, 25 byte, char, ascending.
165800030612     C                   EVAL      QLGSP      = 5
165900030612     C                   EVAL      QLGSS      = %SIZE(vtrasoc)
166000011228     C                   EVAL      QLGDT      = Carattere
166100011228     C                   EVAL      QLGSO      = Ascendente
166200011228     C                   EVAL      QLGKL(1)   = QLGSKL
166300011228     ** Al prossimo F11 ordino per CODICE
166400011228     C                   EVAL      WrkSort = ' '
166500011228     ** Ordinamento per Codice
166600011228     C                   WHEN      WrkSort = ' '
166700011228     C                   EVAL      QLGNBRK    = 1
166800011228     ** Il codice è
166900030612     ** a posizione 31, 25 byte, char, ascending
167000030612     C                   EVAL      QLGSP      = 31
167100030612     C                   EVAL      QLGSS      = %SIZE(vtraaut)
167200011228     C                   EVAL      QLGDT      = Carattere
167300011228     C                   EVAL      QLGSO      = Ascendente
167400011228     C                   EVAL      QLGKL(1)   = QLGSKL
167500011228     ** Al prossimo F11 ordino per descrizione.
167600011228     C                   EVAL      WrkSort = Descrizione
167700011228     C                   ENDSL
167800011228      * Load other sort parameters.
167900011228     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
168000011228     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
168100011228     C                   EVAL      QLGRT     = 8
168200011228     C                   EVAL      QLGOKL    = 80
168300011228     C                   EVAL      QLGLKE    = 16
168400011228     C                   EVAL      QLGLSS    = 290
168500011228      * Initialize Sort I/O API fields.
168600011228     C                   EVAL      QLGRL00  = QLGRL
168700011228     C                   EVAL      QLGRC00  = 1
168800011228     C                   CLEAR                   QUSEI
168900011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
169000011228      * First step - Initialize the sort routine.
169100011228     C                   CALL      'QLGSORT'
169200011228     C                   PARM                    QLGSCB
169300011228     C                   PARM                    NotUsed
169400011228     C                   PARM                    NotUsed
169500011228     C                   PARM                    SizeList
169600011228     C                   PARM                    ReturnSize
169700011228     C                   PARM                    QUSEC
169800011228      * Next step - Write records to I/O routine.
169900011228     C                   EVAL      QLGRT00 = Put
170000011228     C                   DO        RrnLast       x
170100030604     C     x             CHAIN     video2
170200011228     ** Solo le righe con Selected = 'Y' sono riordinate,
170300011228     ** quindi per fare un ordinamento di tutte le righe
170400011228     ** metto 'Y' sempre.
170500011228     C                   EVAL      Selected = 'Y'
170600011228     C                   CLEAR                   QUSEI
170700011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
170800011228     C                   CALL      'QLGSRTIO'
170900011228     C                   PARM                    QLGSCB00
171000011228     C                   PARM                    SflRcd
171100011228     C                   PARM                    NotUsed
171200011228     C                   PARM                    SizeList
171300011228     C                   PARM                    NotUsed
171400011228     C                   PARM                    QUSEC
171500011228     C                   ENDDO
171600011228      * Next step - Signal end of input, clear subfile for reload.
171700011228     C                   EVAL      QLGRT00 = EndPut
171800011228     C                   CLEAR                   QUSEI
171900011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
172000011228
172100011228     C                   CALL      'QLGSRTIO'
172200011228     C                   PARM                    QLGSCB00
172300011228     C                   PARM                    SflRcd
172400011228     C                   PARM                    NotUsed
172500011228     C                   PARM                    SizeList
172600011228     C                   PARM                    NotUsed
172700011228     C                   PARM                    QUSEC
172800011228     c                   seton                                        90
172900030604     C                   WRITE     video2c
173000011228     C                   setoff                                       90
173100011228      * Final step - Write the records back to the subfile.
173200011228     C                   EVAL      QLGRT00 = Get
173300011228     C                   DO        RrnLast       x
173400011228     C                   CLEAR                   QUSEI
173500011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
173600011228     C                   CALL      'QLGSRTIO'
173700011228     C                   PARM                    QLGSCB00
173800011228     C                   PARM                    NotUsed
173900011228     C                   PARM                    SflRcd
174000011228     C                   PARM                    QLGRL00
174100011228     C                   PARM                    NotUsed
174200011228     C                   PARM                    QUSEC
174300030605     c                   z-add     x             snrr1
174400030604     C                   WRITE     video2
174500011228     C                   ENDDO
174600011228     C                   ENDSR
174700090318      * ?___________________________________________________________________
174800000607     C     *INZSR        BEGSR
174900090318      * ?___________________________________________________________________
175000000607      *
175100000607     C     *ENTRY        PLIST
175200000607     C                   PARM                    Kpjba
175300080328      *
175400170905     C                   Z-ADD     1             CODUT             1 0
175500170905     C*m                 CALL      'X§PARUT'
175600170905     C*m                 PARM                    UTEDSE
175700170905     C*m                 MOVEL     REC80         CNCR80
175800110502     c                   eval      vtitolo = '*  Gestione  Autotrasportatori  *'
175900080328      *
176000170904      *Reperimento dati utenti  per autorizzazioni
176001170904     c                   exsr      repdatiute
176002170907     c                   if        dutpou <> 046
176003170907     c                   move      dutpou        v1cfil
176004170907     c                   endif
176005170904      *livello abilitazione per dati autista
176600170907     c                   if        wabiaut <> '2'
176700170907     c                   seton                                        20
176701170907     c                   endif
176702170907      *livello abilitazione per dati mezzo
176703170907     c                   if        wabimez <> '2' and wabimez <> '3'
176704170907     c                   seton                                        3021
176705170907     c                   endif
176800170907      *se manutenzione e profilo filiale proteggo selezione filiale ognuno vede i suoi
176900170907     c                   if        wabi = 'PO'
177000170907     c                   seton                                        18
177100170907     c                   endif
179000170907      * se il codice azione è diverso dall'originale MZ70 non mostro le
179100170907      * opzioni in seguito gestisco solo la visualizzazione
179200171026     c                   if        wabimez = ' ' and wabiaut = ' '
179300170907     c                   eval      vtitolo = '* Interrogaz. Autotrasportatori *'
179400170907     c                   seton                                        202621
179800170907     c                   endif
179900040610     C*-
180000040610     C*   Imposta la data del giorno in formato ISO
180100040610     C                   TIME                    W0140            14 0
180200040610     C                   MOVEl     W0140         Wora              6 0
180300040610     C                   MOVE      W0140         WDAT              8 0
180400040610     C                   Z-ADD     Wdat          G02DAT
180500040610     C                   MOVEL     *BLANK        G02ERR
180600040610     C                   CALL      'XSRDA8'
180700040610     C                   PARM                    WLBDA8
180800040610     C* UDATE A 8 IN AAAA/MM/GG
180900080811     C                   move      G02INV        wdatg             8 0
181000120103     C                   MOVE      wdatg         WDATalfa          8
181100040610     C                   move      G02INV        dataoggi
181200170907      * reperimento autorizzazioni per gestione società o campi riservati
181900170908     c                   setoff                                       010203
182100170907     c                   if        wabisl = '2'
182200170908     c                   seton                                        010203
182300160304     c                   else
182301170907     c                   if        wabisl = '3'
182400170908     c                   seton                                        0203
182500170907     c                   endif
182501171026     c                   if        wabisl = ' '
182502170908     c                   seton                                        03
182503170908     c                   endif
182900170907     c                   endif
183000000607      *
183100030729     C     Kapd          KLIST
183200030729     C                   KFLD                    tipoa             1
183300081126     C                   KFLD                    vkcod             7 0
183400170221      *
183500170221     C     Ktbe          KLIST
183600170221     C                   KFLD                    tbecod
183700170221     C                   KFLD                    tbeke1
183800000607      *---------------------------------------------------------------------
183900080321      *
184000090316     ** Inizializzo il programma. per dati societari
184100090316     C                   CALL      'TIBSSOCR'
184200090316     C                   PARM      'INIT'        prmRqsOpCode
184300090316     C                   PARM                    prmRpyOpCode
184400090316     C                   PARM                    prmRpyIdMsg
184500090316
184600011227     C                   ENDSR
184601170904     C*--------------------------------------------------------------------------------------------*
184602170904     C* REPERISCE I DATI UTENTE
184603170904     C*--------------------------------------------------------------------------------------------*
184604170904     C     REPDATIUTE    BEGSR
184605170904     C*
184606170904     C* INIZIALIZZA VARIABILI DI WRK
184607170904     C                   CLEAR                   TIBS34DS
184608170904     C                   CLEAR                   AZUTEDS
184609170904     C                   CLEAR                   DDATIUTE
184610170904     C*
184611170904     C     *DTAARA       DEFINE    §azute        azuteds
184612170904     C     *DTAARA       DEFINE    §datiute      ddatiute
184613170904     C                   IN(E)     *DTAARA
184614170904     C                   IF        %Error
184615170904     C                   EVAL      I34Tla = 'L'
184616170904     C                   CALL      'TIBS34R'
184617170904     C                   PARM                    Tibs34Ds
184618170904     C                   IN        *DTAARA
184619170904     C                   ENDIF
184620170904     c                   Clear                   wabi
184621170904     c                   Clear                   dLat
184622170904
184623170904      * Verifica errori e autorità profilo
184624170904s   1c                   Select
184625170904      * se ho errori nei dati utente esco dal pgm
184626170904w   1c                   When      DutErr = 'E'
184627170904     c                   seton                                        lr
184628170904     c                   return
184629170904      * se non c'è l'abilitazione
184630170904      * --> se 1° livello, abilitazioni al terminal
184631170904      *     se 2° livello, abilitazioni al punto operativo
184632170904w   1c                   When      UteAut = *Blanks
184633170904if  2c                   If        DutLpo = '1'
184634170904     c                   Eval      wabi   = 'TP'
184635170904e   2c                   EndIf
184636170904if  2c                   If        DutLpo = '2'
184637170904     c                   Eval      wabi   = 'PO'
184638170904e   2c                   EndIf
184639170904if  2c                   If        DutLpo = 'S'
184640170904     c                   Eval      wabi   = 'AZ'
184641170904e   2c                   EndIf
184642171025      * carica le abilitazioni del profilo    GESTIONE
184643170904x   1c                   Other
184644170904     c                   Movel     UteFaf        Dute01
184646170904     c                   If        §Utecerg <> *Blanks
184647170904     c                   Eval      wabi = §utecerg
184648170904     c                   eval      wabiaut = §utecerga
184649170904     c                   eval      wabimez = §utecergm
184650170904     c                   EndIf
184651170904     c                   If        §Utecers <> *Blanks
184652170904     c                   Eval      wabis = §utecers
184653170904     c                   eval      wabisl = §utecersl
184654171026     c                   else
184655171026     c                   Eval      wabis = §utecersv
184656170904     c                   EndIf
184657170904e   1c                   EndSl
184658170904      * controllo se ok l'abilitazione dell'utente
184659170904     c                   Clear                   Tibs02ds
184660170904     c                   Eval      T02Mod = 'C'
184661170904     c                   Eval      T02Sif = knsif
184662170904     c                   Eval      T02Cod = 'LAT'
184663170904     c                   Movel(p)  wabi          T02Ke1
184664170904     c                   Call      'TIBS02R'
184665170904     c                   Parm                    kpjba
184666170904     c                   Parm                    Tibs02ds
184667170904if  1c                   If        T02Err = *Blanks
184668170904     c                   Eval      dLat = T02Uni
184669170904e   1c                   EndIf
184688170904      * Reperimento dei P.O. gestibili dall'utente
184689170904     c                   clear                   TRUL31DS
184690170904     c                   clear                   TRUL31DS2
184691170904     c                   eval      I31abi = wabi
184692170904     c                   eval      I31cdi = DUTdis
184693170904     c                   eval      I31car = DUTare
184694170904     c                   eval      I31cpo = DUTpou
184695170904     c                   call      'TRUL31R'
184696170904     c                   parm                    KPJBA
184697170904     c                   parm                    TRUL31DS
184698170904     c                   parm                    TRUL31DS2
184699170904if  2c                   if        O31pog > *zeros
184700170904     c                   movea     O31pog        $pog
184701170904     c                   movea     O31arg        $arg
184702170904     c                   movea     O31dig        $dig
184707170904    1c                   endif
184708170904     c                   do        250           g                 3 0
184709170904     c                   if        $pog(g) > *zero
184710170904     c                   move      $pog(g)       $pogn(g)
184711170904     c                   else
184712170904     c                   move      999           $pogn(g)
184713170904     c                   endif
184714170904     c                   enddo
184715171025      * ___________                                        ____________________
184716171025      * carica le abilitazioni del profilo    VISUALIZZAZIONE
184717171026     c                   Eval      wabiv = §utecervi
184720171025      * controllo se ok l'abilitazione dell'utente
184721171025     c                   Clear                   Tibs02ds
184722171025     c                   Eval      T02Mod = 'C'
184723171025     c                   Eval      T02Sif = knsif
184724171025     c                   Eval      T02Cod = 'LAT'
184725171025     c                   Movel(p)  wabiv         T02Ke1
184726171025     c                   Call      'TIBS02R'
184727171025     c                   Parm                    kpjba
184728171025     c                   Parm                    Tibs02ds
184729171025if  1c                   If        T02Err = *Blanks
184730171025     c                   Eval      dLat = T02Uni
184731171025e   1c                   EndIf
184732171025      * errore o non abilitato (impossibile xchè controllato nel filtro)
184733171025      * errore o non abilitato (impossibile xchè controllato nel filtro)
184734171025if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S' or
184735171025     c                             wabiv= 'NO' or wabiv= *blank
184736171025     c                   seton                                        98
184737171025     c                   call      'XUTENOAB'
184738171025     c                   parm                    kpjba
184739171025     c                   seton                                        lr
184740171025     c                   return
184741171025   x1c                   endif
184753171025      * Reperimento dei P.O. gestibili dall'utente
184754171025     c                   clear                   TRUL31DS
184755171025     c                   clear                   TRUL31DS2
184756171025     c                   eval      I31abi = wabiv
184757171025     c                   eval      I31cdi = DUTdis
184758171025     c                   eval      I31car = DUTare
184759171025     c                   eval      I31cpo = DUTpou
184760171025     c                   call      'TRUL31R'
184761171025     c                   parm                    KPJBA
184762171025     c                   parm                    TRUL31DS
184763171025     c                   parm                    TRUL31DS2
184764171025if  2c                   if        O31pog > *zeros
184765171025     c                   movea     O31pog        $pog
184766171025     c                   movea     O31arg        $arg
184767171025     c                   movea     O31dig        $dig
184768171025x   2c                   else
184769171025     c* impossibile perchè controllato nel filtrp
184770171025     c                   seton                                        lr
184771171025     c                   return
184772171025    1c                   endif
184773171025     c                   do        250           g                 3 0
184774171025     c                   if        $pog(g) > *zero
184775171025     c                   move      $pog(g)       $pognv(g)
184776171025     c                   else
184777171025     c                   move      999           $pognv(g)
184778171025     c                   endif
184779171025     c                   enddo
184781170904     c                   endsr
184782090318      * ?___________________________________________________________________
184800011227     c     srsql         begsr
184900090318      * ?___________________________________________________________________
185000011227     C                   EVAL      WrkSqlCmd
185100011227     C                             =
185200090317     c                             'SELECT RRN(aitra01l), aitra01l.* -
185300090317     C                             FROM AITRA01L'
185400011227     C                             +
185500030604     C                             ' WHERE TRATIP = ''C'''
185600101227     c*scelta raggruppamento
185700101227      *
185800030604     C                   IF        v1cfil <> 046 and v1cfil <> *zeros
185900011227     C                   EVAL      WrkSqlCmd
186000171025     C                             =
186100011227     C                             %TRIMR(WrkSqlCmd)
186200011227     C                             +
186300030604     C                             ' AND trafil = -
186400030605     C                              ' +  %editc(v1cfil:'Z')
186500011227     C                   endif
186600030617     c*disaccredtati si o no
186700030619     C                   IF        v1cdis = 'N'
186800030617     C                   EVAL      WrkSqlCmd
186900030617     C                             =
187000030617     C                             %TRIMR(WrkSqlCmd)
187100030617     C                             +
187200030619     C                             ' AND tradfi = 0 '
187300030617     C                   endif
187400081126     c*solo autisti città
187500081126     C                   IF        v1taut = 'C'
187600081126     C                   EVAL      WrkSqlCmd
187700081126     C                             =
187800081126     C                             %TRIMR(WrkSqlCmd)
187900081126     C                             +
188000081126     C                             ' AND trakau > 0 '
188100081126     C                   endif
188200090317     c*solo autisti aff/def
188300081126     C                   IF        v1taut = 'A'
188400081126     C                   EVAL      WrkSqlCmd
188500081126     C                             =
188600081126     C                             %TRIMR(WrkSqlCmd)
188700081126     C                             +
188800081126     C                             ' AND trabpt > 0 '
188900081126     C                   endif
189000030604     c*scelta nome autista
189100030604     C                   IF        v1caut <> '*'
189200011227     C                   EVAL      WrkSqlCmd
189300011227     C                             =
189400011227     C                             %TRIMR(WrkSqlCmd)
189500011227     C                             +
189600030604     C                             ' AND traaut like ' +
189700030605     C                             '''' +  %trimr(v1caut) + '%' + ''''
189800011227     C                   endif
189900030604     c*scelta società autista
190000030604     C                   IF        v1csoc <> '*'
190100030604     C                   EVAL      WrkSqlCmd
190200030604     C                             =
190300030604     C                             %TRIMR(WrkSqlCmd)
190400030604     C                             +
190500030604     C                             ' AND trasoc like ' +
190600030605     C                             ''''  + %trimr(v1csoc) + '%' + ''''
190700030604     C                   endif
190800030604     c*scelta nome autista
190900030604     C                   IF        v1ctaa <> '*'
191000030604     C                   EVAL      WrkSqlCmd
191100030604     C                             =
191200030604     C                             %TRIMR(WrkSqlCmd)
191300030604     C                             +
191400030604     C                             ' AND trataa like ' +
191500030604     C                             '''' + '%' + %trimr(v1ctaa) + '%' + ''''
191600030604     C                   endif
191700070523     c*scelta macchina di scorta
191800070523     C                   IF        v1csco <> '*'
191900070523     C                   EVAL      WrkSqlCmd
192000070523     C                             =
192100070523     C                             %TRIMR(WrkSqlCmd)
192200070523     C                             +
192300070523     C                             ' AND trasco like ' +
192400070523     C                             '''' + '%' + %trimr(v1csco) + '%' + ''''
192500070523     C                   endif
192600170221     c*scelta autista senza automezzo       (ex  autista di riserva)
192700070523     C                   IF        v1cris <> '*'
192800070523     C                   EVAL      WrkSqlCmd
192900070523     C                             =
193000070523     C                             %TRIMR(WrkSqlCmd)
193100070523     C                             +
193200170221     C                             ' AND traris <> '' ''  '
193300070523     C                   endif
193400100113     c*scelta autista a perdere
193500100113     C                   IF        v1ccor <> '*'
193600100113     C                   EVAL      WrkSqlCmd
193700100113     C                             =
193800100113     C                             %TRIMR(WrkSqlCmd)
193900100113     C                             +
194000100113     C                             ' AND tracor like ' +
194100100113     C                             '''' + '%' + %trimr(v1ccor) + '%' + ''''
194200100113     C                   endif
194300080327     c*scelta società appartenenza
194400030604     C                   IF        v1csoca <> '*'
194500011227     C                   EVAL      WrkSqlCmd
194600011227     C                             =
194700011227     C                             %TRIMR(WrkSqlCmd)
194800011227     C                             +
194900030604     C                             ' AND trasocg = ' + '''' + v1csoca + ''''
195000011227     C                   endif
195100081202     c*scelta codice autista
195200081202     C                   IF        v1ccod <> 9999999 and v1ccod <> *zeros
195300081202     c                             and v1taut = 'C'
195400081202     C                   EVAL      WrkSqlCmd
195500081202     C                             =
195600081202     C                             %TRIMR(WrkSqlCmd)
195700081202     C                             +
195800081202     C                             ' AND trakau = -
195900081202     C                              ' +  %editc(v1ccod:'Z')
196000081202     C                   endif
196100081202     C                   IF        v1ccod <> 9999999 and v1ccod <> *zeros
196200081202     c                             and v1taut = 'A'
196300081202     C                   EVAL      WrkSqlCmd
196400081202     C                             =
196500081202     C                             %TRIMR(WrkSqlCmd)
196600081202     C                             +
196700081202     C                             ' AND trabpt = -
196800081202     C                              ' +  %editc(v1ccod:'Z')
196900081202     C                   endif
197000030613     C                   EVAL      WrkSqlCmd
197100030613     C                             =
197200030613     C                             %TRIMR(WrkSqlCmd)
197300030613     C                             +
197400030613     C                             ' order by traaut '
197500011227     c*
197600011227     C                   ENDSR
197700080327     c*--------------------------------------------------------------
