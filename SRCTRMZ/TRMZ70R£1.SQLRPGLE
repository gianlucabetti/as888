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
001300090318      *?
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
003600080109
003700080109     D trul31ds      E DS
003800080109     d  SKPO                  10    759  0 dim(250)
003900080109     D trul31ds2     E DS
004000080109
004100081128     D fnlv24ds      E DS
004200090317     D trmz77ds      E DS
004300120103     D dtrsflr       E DS
004400150305AB   D dtrsflr1      E DS
004500090408     D trmz70s1ds    E DS
004600030604     D aitra         E DS                  extname(aitra00f)
004700030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004800030604     D CNCR80        E DS
004900030617     d Tibs36Ds      e ds
005000170221     d dTPA          e ds
005100071113     d daip          e ds
005200081127     d datu          e ds
005300090317     d daua          e ds
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
006600080319      *
006700090316     D tibsSocI0...
006800090316     D               E DS                  QUALIFIED
006900090316     D                                     INZ
007000090316     D tibsSocO0...
007100090316     D               E DS                  QUALIFIED
007200090316     D                                     INZ
007300090316
007400090316      *?
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
010500090318      *?
010600011228      * Constants
010700011228      *     MaxKey - Maximum number of key fields allowed by this program
010800090318      *?
010900030612     D MaxKey          C                   1
011000011228     D Descrizione     C                   '1'
011100011228     D Ascendente      C                   1
011200011228     D Carattere       C                   6
011300011228     D Put             C                   1
011400011228     D EndPut          C                   2
011500011228     D Get             C                   3
011600090318      *?
011700011228      * Standalone fields
011800011228      *     SizeList   - Size of list
011900011228      *     ReturnSize - Size of list returned by sort API
012000090318      *?
012100011228     D NotUsed         S             16A
012200011228     D ReturnSize      S              9B 0
012300011228     D SizeList        S              9B 0
012400011228     D RrnLast         S              5i 0
012500011228     D WrkSort         S              1
012600090318      *?
012700011228      * Data Structures
012800011228      *     SflRcd     - The entire subfile record to pass to the sort
012900011228      *     QLGSCB     - The sort request block for the QLGSORT API
013000011228      *     QLGSCB00   - The sort request block for the QLGSRTIO API
013100011228      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
013200011228      *     QUSEC      - Error structure for the QLGSORT API
013300090318      *?
013400011228     D SflRcd          DS
013500080401     D  v2csce
013600080401     D  v2trasoc
013700080401     D  v2traaut
013800080401     D  v2trafil
013900080401     D  v2trasocg
014000080401     D  v2tradfi
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
015300090318      *?
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
032900060201      * solo per interrogazione abilito a tutto l'anagrafico anche i profili
033000060201      * RED(distretti)
033100060201     c                   if        kcdaz = 'MX70'
033200110114     c*                  if        o36pou <> 046 and %subst(knmus:1:3) <>'RED'
033300110114     c*                            and %subst(knmus:1:3) <>'REA'
033400110114     c*                  move      o36pou        v1cfil
033500110114     c*                  endif
033600090605     c                   if        %subst(knmus:1:3) = 'REA'
033700090605     c     v1cfil        lookup    skpo                                   81
033800090605     c  n81              seton                                          9982
033900090605     c                   endif
034000060201     c                   else
034100110114     C                   IF        o36pou <> 046
034200110114     c                   move      o36pou        v1cfil
034300110114     C                   ENDIF
034400060201     c                   endif
034500081204     c                   if        v1ccod <> 9999999 and v1ccod > *zeros
034600081204     c                             and v1taut = *blank
034700081204     c                   seton                                        8899
034800081204     c                   endif
034900090317      * se premuto F1 richiamo gestione società verifico se profilo autorizzato
035000090317     c                   if        *inka
035100090323     c                   if        §AUAGES = 'S'
035200090326     c                   call      'TRMZ70SC'
035300090317     c                   parm                    kpjba
035400090409     c                   seton                                        99
035500090317     c                   else
035600090319     c                   seton                                        8599
035700090317     c                   endif
035800090317     c                   endif
035900090317      *
036000090323      * se premuto F2 richiamo gestione società x AREA
036100090323     c                   if        *inkb
036200090323     c                   if        §AUAipr = 'S'
036300090323     c                   call      'TRMZ70SR5'
036400090323     c                   parm                    kpjba
036500090409     c                   seton                                        99
036600090323     c                   else
036700090323     c                   seton                                        8599
036800090323     c                   endif
036900090323     c                   endif
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
041400080401     c                   when      v2csce = '2' and kcdaz <> 'MX70'
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
058000070523     c                   move      TRASCO        vTRASCO
058100150324     c                   setoFF                                       58
058200150324      * protegge ITA/EST
058300150324     c                   if        vtraSCO <> ' '
058400150324     c                   setoN                                        58
058500150324     c                   end
058600030605     c                   move      TRATAA        vTRATAA
058700030618     c                   move      TRAcor        vTRAcor
058800030605     c                   move      TRAbpo        vTRAbpo
058900050526     c                   movel     tradass       vtradass
059000081127      * turnover
059100081127     c                   clear                   vtraturd
059200081127     c                   if        vtratur <> *blank
059300081127     c                   clear                   tibs02ds
059400081127     c                   movel     'C'           t02mod
059500081127     c                   movel     knsif         t02sif
059600081127     c                   movel     'ATU'         t02cod
059700081127     c                   movel     vtratur       t02ke1
059800081127     c                   call      'TIBS02R'
059900081127     c                   parm                    KPJBA
060000081127     c                   parm                    TIBS02DS
060100081127     c                   if        t02err =  *blanks
060200081127     c                   movel     t02uni        datu
060300081127     c                   movel     §atudes       vtraturd
060400081127     c                   endif
060500081127     c                   endif
060600030630      * decodifica blocca porte
060700030630     c                   clear                   vtrabpod
060800030630     c                   if        vtrabpo <> *blank
060900030630     c                   clear                   tibs02ds
061000030630     c                   movel     'C'           t02mod
061100030630     c                   movel     knsif         t02sif
061200030630     c                   movel     'ANT'         t02cod
061300030630     c                   movel     'VAN'         t02ke1
061400030630     c                   movel     vtrabpo       t02ke2
061500030630     c                   call      'TIBS02R'
061600030630     c                   parm                    KPJBA
061700030630     c                   parm                    TIBS02DS
061800030630     c                   if        t02err =  *blanks
061900030630     c                   movel     t02uni        vtrabpod
062000030630     c                   endif
062100030630     c                   endif
062200030605     c                   move      TRAANT        vTRAANT
062300160111     c                   movel     TRAper        vTRAper
062400160111     c                   move      TRAcen        vTRAcen
062500090929      *decodifica antifurto
062600090929     c                   clear                   vtraantd
062700090929     c                   if        vtraant <> *blank
062800090929     c                   clear                   tibs02ds
062900090929     c                   movel     'C'           t02mod
063000090929     c                   movel     knsif         t02sif
063100090929     c                   movel     'ANT'         t02cod
063200090929     c                   movel     'MOT'         t02ke1
063300090929     c                   movel     vtraant       t02ke2
063400090929     c                   call      'TIBS02R'
063500090929     c                   parm                    KPJBA
063600090929     c                   parm                    TIBS02DS
063700090929     c                   if        t02err =  *blanks
063800090929     c                   movel     t02uni        vtraantd
063900090929     c                   endif
064000090929     c                   endif
064100090929      *decodifica centrale operativa
064200090929     c                   clear                   vtracend
064300090929     c                   if        vtracen <> *blank
064400030630     c                   clear                   tibs02ds
064500030630     c                   movel     'C'           t02mod
064600030630     c                   movel     knsif         t02sif
064700090929     c                   movel     'CEN'         t02cod
064800090929     c                   movel     vtracen       t02ke1
064900090929     c                   clear                   t02ke2
065000030630     c                   call      'TIBS02R'
065100030630     c                   parm                    KPJBA
065200030630     c                   parm                    TIBS02DS
065300030630     c                   if        t02err =  *blanks
065400090929     c                   movel     t02uni        vtracend
065500030630     c                   endif
065600030630     c                   endif
065700030605     c                   move      TRATMP        vTRATMP
065800031002     c                   setoff                                       23
065900030606     c                   if        tradtm > 0
066000031002     c                   if        o36pou <> 046
066100031002     c                   seton                                        23
066200031002     c                   end
066300040430     c                   move      TRADTM        dataiso
066400040430     c                   move      dataiso       dataeur
066500040430     c                   move      dataeur       vTRADTM
066600040430     c                   else
066700040430     c                   clear                   vtradtm
066800040430     c                   endif
066900031002
067000040430     c                   setoff                                       24
067100040430     c                   if        tradrr > 0
067200040430     c                   if        o36pou <> 046
067300040430     c                   seton                                        24
067400040430     c                   end
067500040430     c                   move      TRADrr        dataiso
067600040430     c                   move      dataiso       dataeur
067700040430     c                   move      dataeur       vTRADrr
067800040430     c                   else
067900040430     c                   clear                   vtradrr
068000040430     c                   endif
068100040430
068200040430     c                   setoff                                       25
068300040430     c                   if        tradpr > 0
068400040430     c                   if        o36pou <> 046
068500040430     c                   seton                                        25
068600040430     c                   end
068700040430     c                   move      TRADpr        dataiso
068800040430     c                   move      dataiso       dataeur
068900040430     c                   move      dataeur       vTRADpr
069000040430     c                   else
069100040430     c                   clear                   vtradpr
069200040430     c                   endif
069300040430
069400030606     c                   if        tradin > 0
069500030606     c                   move      TRADIN        dataiso
069600030606     c                   move      dataiso       dataeur
069700030606     c                   move      dataeur       vTRADin
069800030630     c                   else
069900030630     c                   clear                   vtradin
070000030606     c                   endif
070100030606     c                   if        tradfi > 0
070200030606     c                   move      TRADFI        dataiso
070300030606     c                   move      dataiso       dataeur
070400030606     c                   move      dataeur       vTRADfi
070500030630     c                   else
070600030630     c                   clear                   vtradfi
070700030606     c                   endif
070800030605     c                   move      TRAKAU        vTRAKAU
070900081126     C                   move      'A'           tipoa
071000081126     c                   z-add     vtrakau       vkcod
071100090528     c                   clear                   vtrakaud
071200090528     c     kapd          chain     fiapd01l
071300090528     c                   if        %found(fiapd01l)
071400090528     c                   movel     apdrsf        vtrakaud
071500090528     c                   if        apdatb <> *blank
071600090528     c                   move      ' Annullato'  vtrakaud
071700090528     c                   endif
071800090528     c                   endif
071900080320      *
072000081126     c                   move      TRABPT        vTRABPT
072100081126     C                   move      'D'           tipoa
072200081126     c                   z-add     vtrabpt       vkcod
072300090528     c                   clear                   vtrabptd
072400081126     c     kapd          chain     fiapd01l                           94
072500090528     c                   if        %found(fiapd01l)
072600090528     c                   movel     apdrsf        vtrabptd
072700090528     c                   if        apdatb <> *blank
072800090528     c                   move      ' Annullato'  vtrabptd
072900090528     c                   endif
073000090528     c                   endif
073100150619      *
073200030605     c                   move      TRAPUB        vTRAPUB
073300030605     c                   move      TRAFUR        vTRAFUR
073400070619     c                   move      TRArif        vTRArif
073500120914     c                   move      traloci       vtraloci
073600030630      *decodifica allestimento
073700030630     c                   clear                   vtrafurd
073800030630     c                   if        vtrafur <> *blank
073900030630     c                   clear                   tibs02ds
074000030630     c                   movel     'C'           t02mod
074100030630     c                   movel     knsif         t02sif
074200030630     c                   movel     'ALL'         t02cod
074300030630     c                   movel     vtrafur       t02ke1
074400030630     c                   call      'TIBS02R'
074500030630     c                   parm                    KPJBA
074600030630     c                   parm                    TIBS02DS
074700030630     c                   if        t02err =  *blanks
074800030630     c                   movel     t02uni        vtrafurd
074900030630     c                   endif
075000030630     c                   endif
075100060315      *decodifica classe omologazione
075200060315     c                   move      traff1        vtraff1
075300060315     c                   clear                   vtraff1d
075400060315     c                   if        vtraff1 <> *blank
075500060315     c                   clear                   tibs02ds
075600060315     c                   movel     'C'           t02mod
075700060315     c                   movel     knsif         t02sif
075800060315     c                   movel     'OMO'         t02cod
075900060315     c                   movel     vtraff1       t02ke1
076000060315     c                   call      'TIBS02R'
076100060315     c                   parm                    KPJBA
076200060315     c                   parm                    TIBS02DS
076300060315     c                   if        t02err =  *blanks
076400060315     c                   movel     t02uni        vtraff1d
076500060315     c                   endif
076600060315     c                   endif
076700060315      *decodifica tipo alimentazione
076800060315     c                   move      traff2        vtraff2
076900060315     c                   clear                   vtraff2d
077000060315     c                   if        vtraff2 <> *blank
077100060315     c                   clear                   tibs02ds
077200060315     c                   movel     'C'           t02mod
077300060315     c                   movel     knsif         t02sif
077400060315     c                   movel     'ALI'         t02cod
077500060315     c                   movel     vtraff2       t02ke1
077600060315     c                   call      'TIBS02R'
077700060315     c                   parm                    KPJBA
077800060315     c                   parm                    TIBS02DS
077900060315     c                   if        t02err =  *blanks
078000060315     c                   movel     t02uni        vtraff2d
078100060315     c                   endif
078200060315     c                   endif
078300030605     c                   move      TRASPI        vTRASPI
078400030605     c                   move      TRANT1        vTRANT1
078500030605     c                   move      TRANT2        vTRANT2
078600030606     c                   if        tradt2 > 0
078700030606     c                   move      TRADt2        dataiso
078800030606     c                   move      dataiso       dataeur
078900030606     c                   move      dataeur       vTRADt2
079000030630     c                   else
079100030630     c                   clear                   vtradt2
079200030606     c                   endif
079300030606     c                   if        tradp2 > 0
079400030606     c                   move      TRADP2        dataiso
079500030606     c                   move      dataiso       dataeur
079600030606     c                   move      dataeur       vTRADp2
079700030630     c                   else
079800030630     c                   clear                   vtradp2
079900030606     c                   endif
080000060710     c                   if        tradt3 > 0
080100060710     c                   move      TRADt3        dataiso
080200060710     c                   move      dataiso       dataeur
080300060710     c                   move      dataeur       vTRADt3
080400060710     c                   else
080500060710     c                   clear                   vtradt3
080600060710     c                   endif
080700060710     c                   if        tradt4 > 0
080800060710     c                   move      TRADt4        dataiso
080900060710     c                   move      dataiso       dataeur
081000060710     c                   move      dataeur       vTRADt4
081100060710     c                   else
081200060710     c                   clear                   vtradt4
081300060710     c                   endif
081400030605     c                   move      TRANRP        vTRANRP
081500160114      *  DISabilita il CMD(16) di Disaccr.Affl/Defl.
081600160114     c                   setoff                                       16
081700160114     c                   if        traBPT = 0 or traSCO <> *blank
081800160114     c                   setoN                                        16
081900160114     c                   end
082000030605     c                   exsr      Gesvideo3
082100030605     c                   endif
082200030605     C                   ENDSR
082300090318      * ?___________________________________________________________________
082400030605     C     copia         BEGSR
082500090318      * ?___________________________________________________________________
082600030605     C                   ENDSR
082700090318      * ?___________________________________________________________________
082800030605     C     gesvideo3     BEGSR
082900090318      * ?___________________________________________________________________
083000030605     c                   do        *hival
083100070524     c     sopra         tag
083200030605     c                   exfmt     video3
083300090320     c*                  setoff                                       6358
083400120222      *
083500120222     c                   if        *inkl
083600120222     c                   setoff                                       28
083700120222     c                   leave
083800120222     c                   endif
083900150623      **
084000030919     c                   if        modalita <> '5'
084100030624     c                   exsr      controv3
084200030919     c                   end
084300030729     c   96              iter
084400080423      *
084500071009     c                   if        *inkf and not *in21
084600030729     c                   exsr      aggiorna
084700080423      *
084800030729     c                   leave
084900030729     c                   endif
085000160114      *
085100120222      *disaccreditamento parziale del solo codice di aff/def se doppio codice
085200120222      *e affluenza passata a trazionista
085300120222     c                   if        *inkq and modalita = '2'
085400120222     c                   exsr      disparziale
085500120222     c   96              iter
085600120222     c                   leave
085700120222     c                   endif
085800080423      *
085900080423      *  Se premuto F7 - dati automezzo
086000081201      *  altrimenti forzo l'emissione se profilo di filiale
086100081201     c                   if        not *inkg and o36pou <> 046
086200081201     c                   eval      *inkg = *on
086300081201     c                   endif
086400081201      *
086500030605     c                   if        *inkg
086600080423      *
086700090909     c                   if        vtraris <> *blank or vtracor <> *blank
086800090320     c*                  seton                                        63
086900070523     c                   goto      sopra
087000070523     c                   endif
087100080423      *
087200030605     c                   do        *hival
087300030606     c                   exfmt     video5
087400030605     c   kl              leave
087500030919     c                   if        modalita <> '5'
087600030624     c                   exsr      controv5
087700030919     c                   endif
087800030624     c   95              iter
087900080221     c                   if        *inkf and not *in21
088000030729     c                   exsr      aggiorna
088100030930     c                   goto      endges3
088200030729     c                   endif
088300030605     c                   enddo
088400030605     c                   endif
088500150401      *
088600030605     c                   enddo
088700030930     C     endges3       ENDSR
088800090316      * ?___________________________________________________________________
088900030624     C     Controv3      BEGSR
089000090316      * ?___________________________________________________________________
089100030613      *
089200030624      * *in96 errore generico videata autista
089300120222     c                   setoff                                       9628
089400080320      * -------------------
089500081127      *turnover
089600081127     c                   setoff                                       79
089700081127     c                   clear                   datu
089800090317     c                   clear                   vtraturd
089900081127     c                   if        vtratur = '?'
090000081127     c                   clear                   tibs02ds
090100081127     c                   movel     'R'           t02mod
090200081127     c                   movel     knsif         t02sif
090300081127     c                   movel     'ATU'         t02cod
090400081127     c                   call      'TIBS02R'
090500081127     c                   parm                    KPJBA
090600081127     c                   parm                    TIBS02DS
090700081127     c                   if        t02err =  *blanks
090800081127     c                   movel     t02uni        datu
090900081127     c                   movel     §AtuDES       vtraturd
091000081127     c                   movel     t02ke1        vtratur
091100081127     c                   goto      endctr3
091200081127     c                   else
091300081127     c                   seton                                        79
091400081127     c                   goto      endctr3
091500081127     c                   endif
091600081127     c                   else
091700081127     c                   if        vtratur <> *blank
091800081127     c                   clear                   tibs02ds
091900081127     c                   movel     'C'           t02mod
092000081127     c                   movel     knsif         t02sif
092100081127     c                   movel     'ATU'         t02cod
092200081127     c                   movel     vtratur       t02ke1
092300081127     c                   call      'TIBS02R'
092400081127     c                   parm                    KPJBA
092500081127     c                   parm                    TIBS02DS
092600081127     c                   if        t02err =  *blanks
092700081127     c                   movel     t02uni        datu
092800081127     c                   movel     §AtuDES       vtraturd
092900081127     c                   else
093000081127     c                   seton                                        79
093100081127     c                   goto      endctr3
093200081127     c                   endif
093300081127     c                   endif
093400081127     c                   endif
093500030613      *controlli data  fine rapporto
093600030613     c                   setoff                                       54
093700030613     c                   if        vtradfi > 0
093800030613     C                   MOVE      Vtradfi       G02DAT
093900030613     C                   MOVEL     *BLANK        G02ERR
094000030613     C                   CALL      'XSRDA8'
094100030613     C                   PARM                    WLBDA8
094200030613     C     G02ERR        IFEQ      '1'
094300130710     C                   SETON                                        5496
094400030729     c                   goto      endctr3
094500030613     C                   END
094600030613     c                   move      g02dat        vtradfi
094700160112      * controllo che data fine non sia minore di data accreditamento
094800160112     c                   if        g02inv < tradin
094900160112     C                   SETON                                        5496
095000160112     c                   goto      endctr3
095100160112     c                   endif
095200030613     c                   endif
095300150618      *
095400150619      *
095500030729     C     endctr3       ENDSR
095600090318      * ?___________________________________________________________________
095700030624     C     Controv5      BEGSR
095800090318      * ?___________________________________________________________________
095900030624      *
096000030624      * *in95 errore generico videata automezzo
096100090929     c                   setoff                                       95
096200030624      *targa  obbligatoria se gestione filiale
096300030624     c                   setoff                                       65
096400030624     c                   if        vtrataa = *blank and o36pou <> 046
096500030624     c                   seton                                        6595
096600030729     c                   goto      endctr5
096700030624     c                   endif
096800050524      *data immatricolazione obbligatoria solo segnalazione
096900050524     c                   setoff                                       7273
097000050524     c                   if        vtradass = *zeros and o36pou <> 046
097100050524     c                   seton                                        72
097200050524     c                   endif
097300050527     c                   if        vtradass > *zeros and vtradass < 1900
097400050527     c                   seton                                        73
097500050527     c                   goto      endctr5
097600050527     c                   endif
097700030624      *modello mezzo obbligatorio se gestione filiale
097800030624     c                   setoff                                       66
097900030624     c                   if        vtratia = *blank and o36pou <> 046
098000030624     c                   seton                                        6695
098100030729     c                   goto      endctr5
098200030624     c                   endif
098300030624      *allestimento mezzo obbligatorio se gestione filiale
098400030630     c                   setoff                                       6745
098500030624     c                   if        vtrafur = *blank and o36pou <> 046
098600030624     c                   seton                                        6795
098700030729     c                   goto      endctr5
098800030624     c                   endif
098900030624      *allestimento (tabella)
099000030624     c                   if        vtrafur = '?'
099100030624     c                   clear                   tibs02ds
099200030624     c                   movel     'R'           t02mod
099300030624     c                   movel     knsif         t02sif
099400030624     c                   movel     'ALL'         t02cod
099500030624     c                   call      'TIBS02R'
099600030624     c                   parm                    KPJBA
099700030624     c                   parm                    TIBS02DS
099800030624     c                   if        t02err =  *blanks
099900030624     c                   movel     t02uni        vtrafurd
100000030624     c                   movel     t02ke1        vtrafur
100100030624     c                   goto      endctr5
100200030624     c                   endif
100300030624     c                   else
100400030624     c                   if        vtrafur <> *blank
100500071114     c                   clear                   tibs02ds
100600030624     c                   movel     'C'           t02mod
100700030624     c                   movel     knsif         t02sif
100800030624     c                   movel     'ALL'         t02cod
100900030624     c                   movel     vtrafur       t02ke1
101000030624     c                   call      'TIBS02R'
101100030624     c                   parm                    KPJBA
101200030624     c                   parm                    TIBS02DS
101300030624     c                   if        t02err =  *blanks
101400030624     c                   movel     t02uni        vtrafurd
101500030624     c                   else
101600030624     c                   seton                                        4595
101700030729     c                   goto      endctr5
101800030624     c                   endif
101900030624     c                   endif
102000030624     c                   endif
102100030624      *sponda idraulica  obbligatoria se gestione filiale
102200030624     c                   setoff                                       68
102300030624     c                   if        vtraspi = *blank and o36pou <> 046
102400030624     c                   seton                                        6895
102500030729     c                   goto      endctr5
102600030624     c                   endif
102700030624      *colore  mezzo obbligatorio se gestione filiale
102800030624     c                   setoff                                       69
102900030624     c                   if        vtratmp = *blank and o36pou <> 046
103000030624     c                   seton                                        6995
103100030729     c                   goto      endctr5
103200030624     c                   endif
103300030624      *pubblicità mezzo obbligatoria se gestione filiale
103400030624     c                   setoff                                       70
103500030624     c                   if        vtrapub = *blank and o36pou <> 046
103600030624     c                   seton                                        7095
103700030729     c                   goto      endctr5
103800030624     c                   endif
103900090318      *rifrangente posteriore obbligatorio se gestione filiale
104000090318     c                   setoff                                       71
104100090318     c                   if        vtrarif = *blank and o36pou <> 046
104200090318     c                   seton                                        7195
104300090318     c                   goto      endctr5
104400090318     c                   endif
104500030624      * antifurto vano carico
104600030624     c                   setoff                                       46
104700081201      *antifurto vano carico obbligatorio se gestione filiale
104800081201     c                   if        vtrabpo = *blank and o36pou <> 046
104900081201     c                   seton                                        4695
105000081201     c                   goto      endctr5
105100081201     c                   endif
105200030624     c                   if        vtrabpo = '?  '
105300030624     c                   clear                   tibs02ds
105400030624     c                   movel     'R'           t02mod
105500030624     c                   movel     knsif         t02sif
105600030624     c                   movel     'ANT'         t02cod
105700030624     c                   movel(p)  'VAN'         t02ke1
105800030624     c                   call      'TIBS02R'
105900030624     c                   parm                    KPJBA
106000030624     c                   parm                    TIBS02DS
106100030624     c                   if        t02err =  *blanks
106200030624     c                   movel     t02uni        vtrabpod
106300030624     c                   movel     t02ke2        vtrabpo
106400030624     c                   endif
106500030624     c                   else
106600030624     c                   if        vtrabpo <> *blank
106700030624     c                   clear                   tibs02ds
106800030624     c                   movel     'C'           t02mod
106900030624     c                   movel     knsif         t02sif
107000030624     c                   movel     'ANT'         t02cod
107100030624     c                   movel     'VAN'         t02ke1
107200030624     c                   movel     vtrabpo       t02ke2
107300030624     c                   call      'TIBS02R'
107400030624     c                   parm                    KPJBA
107500030624     c                   parm                    TIBS02DS
107600030624     c                   if        t02err =  *blanks
107700030624     c                   movel     t02uni        vtrabpod
107800030624     c                   else
107900030624     c                   seton                                        4695
108000030729     c                   goto      endctr5
108100030624     c                   endif
108200030624     c                   endif
108300030624     c                   endif
108400080826      *
108500030624      * antifurto blocco motore
108600030624     c                   setoff                                       47
108700081201      *antifurto blocco motore obbligatorio se gestione filiale
108800081201     c                   if        vtraant = *blank and o36pou <> 046
108900081201     c                   seton                                        4795
109000081201     c                   goto      endctr5
109100081201     c                   endif
109200030624     c                   if        vtraant = '?  '
109300030624     c                   clear                   tibs02ds
109400030624     c                   movel     'R'           t02mod
109500030624     c                   movel     knsif         t02sif
109600030624     c                   movel     'ANT'         t02cod
109700030624     c                   movel(p)  'MOT'         t02ke1
109800030624     c                   call      'TIBS02R'
109900030624     c                   parm                    KPJBA
110000030624     c                   parm                    TIBS02DS
110100030624     c                   if        t02err =  *blanks
110200030624     c                   movel     t02uni        vtraantd
110300030624     c                   movel     t02ke2        vtraant
110400030624     c                   endif
110500030624     c                   else
110600030624     c                   if        vtraant <> *blank
110700030624     c                   clear                   tibs02ds
110800030624     c                   movel     'C'           t02mod
110900030624     c                   movel     knsif         t02sif
111000030624     c                   movel     'ANT'         t02cod
111100030624     c                   movel     'MOT'         t02ke1
111200030624     c                   movel     vtraant       t02ke2
111300030624     c                   call      'TIBS02R'
111400030624     c                   parm                    KPJBA
111500030624     c                   parm                    TIBS02DS
111600030624     c                   if        t02err =  *blanks
111700030624     c                   movel     t02uni        vtraantd
111800030624     c                   else
111900030624     c                   seton                                        4795
112000030729     c                   goto      endctr5
112100030624     c                   endif
112200030624     c                   endif
112300030624     c                   endif
112400090929      * centrali operative satellitari
112500090929     c                   setoff                                       41
112600090929     c                   if        vtracen = '?  '
112700090929     c                   clear                   tibs02ds
112800090929     c                   movel     'R'           t02mod
112900090929     c                   movel     knsif         t02sif
113000090929     c                   movel     'CEN'         t02cod
113100090929     c                   call      'TIBS02R'
113200090929     c                   parm                    KPJBA
113300090929     c                   parm                    TIBS02DS
113400090929     c                   if        t02err =  *blanks
113500090929     c                   movel     t02uni        vtracend
113600090929     c                   movel     t02ke1        vtracen
113700090929     c                   endif
113800090929     c                   else
113900090929     c                   if        vtracen <> *blank
114000090929     c                   clear                   tibs02ds
114100090929     c                   movel     'C'           t02mod
114200090929     c                   movel     knsif         t02sif
114300090929     c                   movel     'CEN'         t02cod
114400090929     c                   movel     vtracen       t02ke1
114500090929     c                   call      'TIBS02R'
114600090929     c                   parm                    KPJBA
114700090929     c                   parm                    TIBS02DS
114800090929     c                   if        t02err =  *blanks
114900090929     c                   movel     t02uni        vtracend
115000090929     c                   else
115100090929     c                   seton                                        4195
115200090929     c                   goto      endctr5
115300090929     c                   endif
115400090929     c                   endif
115500090929     c                   endif
115600060315      *Classe omologazione (tabella)
115700060315     c                   setoff                                       31
115800081201     c                   clear                   vtraff1d
115900081201      *Classe omologazione  obbligatorio se gestione filiale
116000081201     c                   if        vtraff1 = *blank and o36pou <> 046
116100081201     c                   seton                                        3195
116200081201     c                   goto      endctr5
116300081201     c                   endif
116400060315     c                   if        vtraff1 = '?'
116500060315     c                   clear                   tibs02ds
116600060315     c                   movel     'R'           t02mod
116700060315     c                   movel     knsif         t02sif
116800060315     c                   movel     'OMO'         t02cod
116900060315     c                   call      'TIBS02R'
117000060315     c                   parm                    KPJBA
117100060315     c                   parm                    TIBS02DS
117200060315     c                   if        t02err =  *blanks
117300060315     c                   movel     t02uni        vtraff1d
117400060315     c                   movel     t02ke1        vtraff1
117500060315     c                   goto      endctr5
117600060315     c                   endif
117700060315     c                   else
117800060315     c                   if        vtraff1 <> *blank
117900060315     c                   clear                   tibs02ds
118000060315     c                   movel     'C'           t02mod
118100060315     c                   movel     knsif         t02sif
118200060315     c                   movel     'OMO'         t02cod
118300060315     c                   movel     vtraff1       t02ke1
118400060315     c                   call      'TIBS02R'
118500060315     c                   parm                    KPJBA
118600060315     c                   parm                    TIBS02DS
118700060315     c                   if        t02err =  *blanks
118800060315     c                   movel     t02uni        vtraff1d
118900060315     c                   else
119000060315     c                   seton                                        3195
119100060315     c                   goto      endctr5
119200060315     c                   endif
119300060315     c                   endif
119400060315     c                   endif
119500060315      *tipo alimentazione (tabella)
119600060315     c                   setoff                                       32
119700090318      *tipo alimentazione  obbligatorio se gestione filiale
119800081201     c                   if        vtraff2 = *blank and o36pou <> 046
119900081201     c                   seton                                        3295
120000081201     c                   goto      endctr5
120100081201     c                   endif
120200070208     c                   clear                   vtraff2d
120300060315     c                   if        vtraff2 = '?'
120400060315     c                   clear                   tibs02ds
120500060315     c                   movel     'R'           t02mod
120600060315     c                   movel     knsif         t02sif
120700060315     c                   movel     'ALI'         t02cod
120800060315     c                   call      'TIBS02R'
120900060315     c                   parm                    KPJBA
121000060315     c                   parm                    TIBS02DS
121100060315     c                   if        t02err =  *blanks
121200060315     c                   movel     t02uni        vtraff2d
121300060315     c                   movel     t02ke1        vtraff2
121400060315     c                   goto      endctr5
121500060315     c                   endif
121600060315     c                   else
121700060315     c                   if        vtraff2 <> *blank
121800060315     c                   clear                   tibs02ds
121900060315     c                   movel     'C'           t02mod
122000060315     c                   movel     knsif         t02sif
122100060315     c                   movel     'ALI'         t02cod
122200060315     c                   movel     vtraff2       t02ke1
122300060315     c                   call      'TIBS02R'
122400060315     c                   parm                    KPJBA
122500060315     c                   parm                    TIBS02DS
122600060315     c                   if        t02err =  *blanks
122700060315     c                   movel     t02uni        vtraff2d
122800060315     c                   else
122900060315     c                   seton                                        3295
123000060315     c                   goto      endctr5
123100060315     c                   endif
123200060315     c                   endif
123300060315     c                   endif
123400030624      *controlli data  verniciatura
123500030624     c                   setoff                                       56
123600030624     c                   if        vtradtm > 0
123700030624     C                   MOVE      Vtradtm       G02DAT
123800030624     C                   MOVEL     *BLANK        G02ERR
123900030624     C                   CALL      'XSRDA8'
124000030624     C                   PARM                    WLBDA8
124100030624     C     G02ERR        IFEQ      '1'
124200030624     C                   SETON                                        5695
124300030729     c                   goto      endctr5
124400030624     C                   END
124500030624     c                   move      g02dat        vtradtm
124600030624     c                   endif
124700030624      *controlli data  verniciatura 2
124800030624     c                   setoff                                       57
124900030624     c                   if        vtradt2 > 0
125000030624     C                   MOVE      Vtradt2       G02DAT
125100030624     C                   MOVEL     *BLANK        G02ERR
125200030624     C                   CALL      'XSRDA8'
125300030624     C                   PARM                    WLBDA8
125400030624     C     G02ERR        IFEQ      '1'
125500030624     C                   SETON                                        5795
125600030729     c                   goto      endctr5
125700030624     C                   END
125800030624     c                   move      g02dat        vtradt2
125900030624     c                   endif
126000030624      *controlli data  ispezione mezzo
126100040610     c                   setoff                                       6064
126200030624     c                   if        vtradrr > 0
126300030624     C                   MOVE      Vtradrr       G02DAT
126400030624     C                   MOVEL     *BLANK        G02ERR
126500030624     C                   CALL      'XSRDA8'
126600030624     C                   PARM                    WLBDA8
126700030624     C     G02ERR        IFEQ      '1'
126800030624     C                   SETON                                        6095
126900030729     c                   goto      endctr5
127000030624     C                   END
127100040610      *- Nuovo controllo la data immessa deve essere quella del giorno e non altro
127200040610     C                   move      G02INV        datawday
127300040628     C                   IF        datawday <> dataoggi  and
127400040628     c                             *in21 = *off and *in24 = *off
127500040611     c                   if        o36pou <> 046
127600040610     C                   SETON                                        6495
127700040610     c                   goto      endctr5
127800040610     C                   END
127900040611     C                   END
128000040610      *-
128100030624     c                   move      g02dat        vtradrr
128200030624     c                   endif
128300030624      *controlli data  previsto ripristino
128400090320     c                   setoff                                       6163
128500030624     c                   if        vtradpr > 0
128600090320      * non si può inserire la data ripristino se la data ispezione è a 0
128700090320     c                   if        vtradrr = 0
128800090320     C                   seton                                        6395
128900090320     c                   goto      endctr5
129000090320     C                   END
129100030624     C                   MOVE      Vtradpr       G02DAT
129200030624     C                   MOVEL     *BLANK        G02ERR
129300030624     C                   CALL      'XSRDA8'
129400030624     C                   PARM                    WLBDA8
129500030624     C     G02ERR        IFEQ      '1'
129600090320     C                   SETON                                        6195
129700030729     c                   goto      endctr5
129800030624     C                   END
129900030624     c                   move      g02dat        vtradpr
130000030624     c                   endif
130100030624      *controlli data  ripristino 2
130200090320     c                   setoff                                       6277
130300030624     c                   if        vtradp2 > 0
130400090320     c                   if        vtradrr = 0
130500090320     C                   SETON                                        7795
130600090320     c                   goto      endctr5
130700090320     C                   END
130800030624     C                   MOVE      Vtradp2       G02DAT
130900030624     C                   MOVEL     *BLANK        G02ERR
131000030624     C                   CALL      'XSRDA8'
131100030624     C                   PARM                    WLBDA8
131200030624     C     G02ERR        IFEQ      '1'
131300030624     C                   SETON                                        6295
131400030729     c                   goto      endctr5
131500030624     C                   END
131600030624     c                   move      g02dat        vtradp2
131700030624     c                   endif
131800060619      *controlli data  scadenza RCV
131900060619     c                   setoff                                       76
132000060619     c                   if        vtradt3 > 0
132100060619     C                   MOVE      Vtradt3       G02DAT
132200060619     C                   MOVEL     *BLANK        G02ERR
132300060619     C                   CALL      'XSRDA8'
132400060619     C                   PARM                    WLBDA8
132500060619     C     G02ERR        IFEQ      '1'
132600060619     C                   SETON                                        7695
132700060619     c                   goto      endctr5
132800060619     C                   END
132900060619     c                   move      g02dat        vtradt3
133000060619     c                   endif
133100060619      *controlli data  disdetta RCV
133200060619     c                   setoff                                       75
133300060619     c                   if        vtradt4 > 0
133400060619     C                   MOVE      Vtradt4       G02DAT
133500060619     C                   MOVEL     *BLANK        G02ERR
133600060619     C                   CALL      'XSRDA8'
133700060619     C                   PARM                    WLBDA8
133800060619     C     G02ERR        IFEQ      '1'
133900060619     C                   SETON                                        7595
134000060619     c                   goto      endctr5
134100060619     C                   END
134200060619     c                   move      g02dat        vtradt4
134300060619     c                   endif
134400030624     C     endctr5       ENDSR
134500120222      * ?___________________________________________________________________
134600120222     C     disparziale   BEGSR
134700120222      * ?___________________________________________________________________
134800120222     c                   if        vtrabpt = 0
134900120222     c                   eval      $msg = 'Comando non eseguibile codice -
135000120222     c                             AF/DE non valorizzato'
135100120222     c                   seton                                        2896
135200120222     c                   leavesr
135300120222     c                   endif
135400120222     c                   if        vtrabpt > 0 and vtrakau = 0
135500120222     c                   eval      $msg = 'Comando non eseguibile codice -
135600120222     c                             città non valorizzato disaccreditare -
135700120222     c                             definitivamente'
135800120222     c                   seton                                        2896
135900120222     c                   leavesr
136000120222     c                   endif
136100120222     c                   if        vtradfi = 0
136200120222     c                   eval      $msg = 'Comando non eseguibile data -
136300120222     c                             disaccreditamento non impostata'
136400120222     c                   seton                                        2896
136500120222     c                   leavesr
136600120222     c                   else
136700120222     C                   MOVE      Vtradfi       G02DAT
136800120222     C                   MOVEL     *BLANK        G02ERR
136900120222     C                   CALL      'XSRDA8'
137000120222     C                   PARM                    WLBDA8
137100120222     C     G02ERR        IFEQ      '1'
137200120222     C                   SETON                                        5495
137300120222     c                   leavesr
137400120222     C                   END
137500120222     c                   endif
137600120222      *aggiorna record in linea eliminando codice aff/def
137700120222     c     v2nrrn        chain     aitra00f
137800120222     c                   if        %found(aitra00f)
137900120222      * gestione data tecnoblock
138000120222     c                   move      *zeros        trabpt
138100120222     c                   update    aitra000
138200120222      *scrive record disaccreditato con codice aff/def
138300120222     c                   move      *zeros        trakau
138400120222     c                   move      vtrabpt       trabpt
138500120222     c                   move      '8'           tratur
138600120222     c                   move      g02inv        tradfi
138700120222     c                   write     aitra000
138800120222     c                   endif
138900120222      *
139000120222     C                   ENDSR
139100090318      * ?___________________________________________________________________
139200030605     C     aggiorna      BEGSR
139300090318      * ?___________________________________________________________________
139400070524      * campi video3
139500090316     c                   if        modalita = '2'
139600090318     c     v2nrrn        chain     aitra00f
139700090316     c                   if        %found(aitra00f)
139800070524     c                   if        vtradfi > 0
139900070524     c                   move      vTRADFI       dataeur
140000070524     c                   move      dataeur       dataiso
140100070524     c                   move      dataiso       TRADfi
140200070524     c                   endif
140300070528      * se impostato autista di riserva abblenca dati automezzo
140400090909     c                   if        vtraris <> *blank  or vtracor <> *blank
140500070528     c                   clear                   video5
140600091014     c                   if        tratmp <> *blank
140700070528     c                   move      'S'           vtratmp
140800091014     c                   endif
140900070528     c                   endif
141000090417     c                   move      vTRAtur       TRAtur
141100070524      * campi video5
141200030610     c                   move      vTRAbpo       TRAbpo
141300030610     c                   move      vTRAANT       TRAANT
141400160111     c                   move      vTRAcen       TRAcen
141500160111     c                   movel     vTRAper       TRAper
141600030610     c                   move      vTRATMP       TRATMP
141700070524     c                   move      vTRATIA       TRATIA
141800070524     c                   move      vTRATAA       TRATAA
141900050524     c                   if        vtradass > 0
142000050526     c                   movel     vTRAdass      tradass
142100050526     c                   move      0101          tradass
142200050524     c                   else
142300050524     c                   clear                   tradass
142400050524     c                   endif
142500030610     c                   if        vtradtm > 0
142600030610     c                   move      vTRADTM       dataeur
142700030610     c                   move      dataeur       dataiso
142800030610     c                   move      dataiso       TRADTM
142900031003     c                   else
143000031003     c                   clear                   tradtm
143100030610     c                   endif
143200030610     c                   move      vTRAPUB       TRAPUB
143300030610     c                   move      vTRAFUR       TRAFUR
143400070619     c                   move      vTRArif       TRArif
143500030610     c                   move      vTRASPI       TRASPI
143600030610     c                   move      vTRANT1       TRANT1
143700030610     c                   move      vTRANT2       TRANT2
143800060315     c                   move      vTRAff1       TRAff1
143900060315     c                   move      vTRAff2       TRAff2
144000120914     c                   move      vTRAloci      TRAloci
144100150309      *
144200030610     c                   if        vtradt2 > 0
144300030610     c                   move      vTRADt2       dataeur
144400030610     c                   move      dataeur       dataiso
144500030610     c                   move      dataiso       TRADt2
144600031003     c                   else
144700031003     c                   clear                   tradt2
144800030610     c                   endif
144900030610     c                   if        vtradrr > 0
145000030610     c                   move      vTRADRR       dataeur
145100030610     c                   move      dataeur       dataiso
145200030610     c                   move      dataiso       TRADrr
145300031003     c                   else
145400031003     c                   clear                   tradrr
145500030610     c                   endif
145600030610     c                   if        vtradpr > 0
145700030610     c                   move      vTRADPR       dataeur
145800030610     c                   move      dataeur       dataiso
145900030610     c                   move      dataiso       TRADpr
146000031003     c                   else
146100031003     c                   clear                   tradpr
146200030610     c                   endif
146300030610     c                   if        vtradp2 > 0
146400030610     c                   move      vTRADP2       dataeur
146500030610     c                   move      dataeur       dataiso
146600030610     c                   move      dataiso       TRADp2
146700031003     c                   else
146800031003     c                   clear                   tradp2
146900030610     c                   endif
147000060619     c                   if        vtradt3 > 0
147100060619     c                   move      vTRADt3       dataeur
147200060619     c                   move      dataeur       dataiso
147300060619     c                   move      dataiso       TRADt3
147400060619     c                   else
147500060619     c                   clear                   tradt3
147600060619     c                   endif
147700060619     c                   if        vtradt4 > 0
147800060619     c                   move      vTRADt4       dataeur
147900060619     c                   move      dataeur       dataiso
148000060619     c                   move      dataiso       TRADt4
148100060619     c                   else
148200060619     c                   clear                   tradt4
148300060619     c                   endif
148400030610     c                   move      vTRANRP       TRANRP
148500150616     c                   eval      tradp4    = wdatg
148600150616      **
148700030610     c                   update    aitra000
148800080901     c                   endif
148900030610     c                   endif
149000030605     C                   ENDSR
149100090318      * ?___________________________________________________________________
149200011228     c* carica sfl
149300090318      * ?___________________________________________________________________
149400030604     C     Carvideo2     BEGSR
149500011228     C                   IF        WrkCarS01 = *ON
149600030604     C                   EVAL      snrr1   = 0
149700011228     C                   EVAL      *IN90 = *ON
149800020219     C                   EVAL      *IN91 = *OFF
149900030604     C                   WRITE     video2c
150000011228     C                   EVAL      *IN90 = *OFF
150100080328     C                   EVAL      dknmus  = knmus
150200080328     C                   EVAL      dknsif  = knsif
150300030604     c* Lettura aitra01l
150400011228     C/EXEC SQL
150500011228     C+ PREPARE S1 FROM :WrkSqlCmd
150600011228     C/END-EXEC
150700011228
150800011228     C/EXEC SQL
150900011228     C+ DECLARE A1 CURSOR FOR S1
151000011228     C/END-EXEC
151100011228
151200011228     C/EXEC SQL
151300011228     C+ OPEN A1
151400011228     C/END-EXEC
151500011228
151600011228     C                   DOU       SqlCod <> 0
151700011228     C/EXEC SQL
151800090317     C+ FETCH NEXT FROM A1 INTO :rrnaitra, :aitra
151900011228     C/END-EXEC
152000011228     c*
152100011228     C                   SELECT
152200011228     C                   WHEN      SqlCod = 0
152300080401     c                   movel     *blanks       v2csce
152400090317     c                   movel     trasoc        v2trasoc
152500090317     c                   movel     traaut        v2traaut
152600090317     c                   movel     trafil        v2trafil
152700090317     c                   movel     trasocg       v2trasocg
152800080401     c                   clear                   v2tradfi
152900090317     c                   z-add     rrnaitra      v2nrrn
153000030729     c                   if        tradfi > 0
153100030729     c                   move      tradfi        dataiso
153200030729     c                   move      dataiso       dataeur
153300080401     c                   move      dataeur       v2tradfi
153400030729     c                   endif
153500030617      *
153600030617      *esco per raggiunta massima capacità sfl
153700030617     c                   if        snrr1 > 9990
153800030617     c                   leave
153900030617     c                   endif
154000030617
154100030604     C                   EVAL      snrr1 = snrr1 + 1
154200030604     C                   WRITE     video2
154300011228     C                   WHEN      SqlCod = 100
154400011228     C                   WHEN      SqlCod <> 0
154500011228      * Forzo la stampa del JOBLOG.
154600011228     C                   CALL      'X66CHGJOB'
154700011228     C                   ENDSL
154800011228     C                   ENDDO
154900011228     C/EXEC SQL
155000011228     C+ CLOSE A1
155100011228     C/END-EXEC
155200030604     C                   IF        snrr1  > 0
155300020219     C                   EVAL      *IN91 = *ON
155400020219     C                   MOVE      1             OK                1 0
155500020219     C                   ELSE
155600020219     C                   MOVE      0             OK                1 0
155700020219     C                   ENDIF
155800071119     c*
155900030604     C                   EVAL      RrnLast = snrr1
156000011228     C                   EVAL      WrkSort = ' '
156100030612     c*                  exsr      sort
156200030612     C                   EVAL      nrr1  = 1
156300011228     C                   ENDIF
156400080401     C*****              EVAL      WrkCarS01 = *OFF
156500030604     C                   EVAL      snrr1  = 1
156600030604     C                   EVAL      csrrrn = 1
156700011228     C                   ENDSR
156800090318      * ?___________________________________________________________________
156900011227     c* gestione sfl
157000090318      * ?___________________________________________________________________
157100000613     C     GestioneSFL   BEGSR
157200000627      *
157300000627     C                   EVAL      WrkEofS01 = *OFF
157400030605      * Elaborazione opzioni.
157500011228     c*
157600080402     c                   z-add     0             last_rig_opz      4 0
157700030605     c                   do        *hival        X                 4 0
157800030605     c     X             CHAIN     video2                             50
157900030605     C   50              LEAVE
158000080513     c                   clear                   modalita
158100080522     c                   setoff                                       38
158200080401     C                   IF        v2csce <> *blanks
158300030605     c                   exsr      opzione
158400080402     c                   z-add     x             last_rig_opz
158500080401     c                   move      *blank        v2csce
158600030605     c                   update    video2
158700030611     c                   setoff                                       22
158800030611
158900011228     C                   ENDIF
159000030605     C                   enddo
159100000627      *
159200000613     C                   ENDSR
159300090318      * ?___________________________________________________________________
159400090318     C     RagSocCont    BegSR
159500090318      * ?___________________________________________________________________
159600090318      *
159700090408     C                   clear                   trmz70s1ds
159800090320     C                   movel(p)  TRSiva        PartitaIVA                     Input
159900090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
160000090320     C                   movel(p)  TRSSOCG       Societa                        Input/Output
160100090320     C                   movel(p)  TRSFIL        Unita                          Input/Output
160200090318      *
160300090320     c                   call      'TRMZ70SR1'
160400090408     C                   parm                    trmz70s1ds
160500090318     c*
160600090320     C                   ENDSR
160700090318      * ?___________________________________________________________________
160800000703      * Ripetizione opzione in tutte le righe del subfile.
160900090318      * ?___________________________________________________________________
161000000703     C     RipetiOpz     BEGSR
161100000703      *
161200030604     C                   IF        CsrRrn > *ZERO
161300030604     C                   EVAL      Snrr1 = CsrRrn
161400030604     C     snrr1         CHAIN     video2
161500080401     C                   IF        %FOUND and v2csce <> *blanks
161600080401     C                   EVAL      SavOpzione = v2csce
161700030604     C                   UPDATE    video2
161800000703     C                   EVAL      WrkEofS01 = *OFF
161900000703     C                   DOU       WrkEofS01 = *ON
162000030604     C                   ADD       1             snrr1
162100030604     C     snrr1         CHAIN     video2
162200000703     C                   IF        %FOUND
162300080401     C                   EVAL      v2csce = SavOpzione
162400030604     C                   UPDATE    video2
162500000703     C                   ELSE
162600000703     C                   EVAL      WrkEofS01 = *ON
162700000703     C                   ENDIF
162800000703     C                   ENDDO
162900000703     C                   ENDIF
163000000703     C                   ENDIF
163100000703      *
163200000703     C                   ENDSR
163300090318      * ?___________________________________________________________________
163400011228      * Subroutine - Sort
163500011228      *   This subroutine sorts the subfile records.
163600090318      * ?___________________________________________________________________
163700011228     C     Sort          BEGSR
163800080401     C                   CLEAR                   v2traaut
163900080401     C                   CLEAR                   v2trasoc
164000030604     C                   EVAL      nrr1  = 1
164100011228
164200011228      * Initialize the key fields to sort on. There is one extra field not in
164300011228      * the subfile -- Selected -- that is added to the record. This field
164400011228      * is used to place selected records in front of nonselected records.
164500011228
164600011228     C                   CLEAR                   QLGSCB
164700011228     C                   CLEAR                   QLGSCB00
164800011228
164900011228     ** Gestione della scelta ordinamento.
165000011228     C                   SELECT
165100011228     ** Ordinamento per descrizione.
165200011228     C                   WHEN      WrkSort = Descrizione
165300011228     ** 1 campo chiave.
165400011228     C                   EVAL      QLGNBRK    = 1
165500011228     ** La descrizione è
165600030612     ** a posizone 5, 25 byte, char, ascending.
165700030612     C                   EVAL      QLGSP      = 5
165800030612     C                   EVAL      QLGSS      = %SIZE(vtrasoc)
165900011228     C                   EVAL      QLGDT      = Carattere
166000011228     C                   EVAL      QLGSO      = Ascendente
166100011228     C                   EVAL      QLGKL(1)   = QLGSKL
166200011228     ** Al prossimo F11 ordino per CODICE
166300011228     C                   EVAL      WrkSort = ' '
166400011228     ** Ordinamento per Codice
166500011228     C                   WHEN      WrkSort = ' '
166600011228     C                   EVAL      QLGNBRK    = 1
166700011228     ** Il codice è
166800030612     ** a posizione 31, 25 byte, char, ascending
166900030612     C                   EVAL      QLGSP      = 31
167000030612     C                   EVAL      QLGSS      = %SIZE(vtraaut)
167100011228     C                   EVAL      QLGDT      = Carattere
167200011228     C                   EVAL      QLGSO      = Ascendente
167300011228     C                   EVAL      QLGKL(1)   = QLGSKL
167400011228     ** Al prossimo F11 ordino per descrizione.
167500011228     C                   EVAL      WrkSort = Descrizione
167600011228     C                   ENDSL
167700011228      * Load other sort parameters.
167800011228     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
167900011228     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
168000011228     C                   EVAL      QLGRT     = 8
168100011228     C                   EVAL      QLGOKL    = 80
168200011228     C                   EVAL      QLGLKE    = 16
168300011228     C                   EVAL      QLGLSS    = 290
168400011228      * Initialize Sort I/O API fields.
168500011228     C                   EVAL      QLGRL00  = QLGRL
168600011228     C                   EVAL      QLGRC00  = 1
168700011228     C                   CLEAR                   QUSEI
168800011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
168900011228      * First step - Initialize the sort routine.
169000011228     C                   CALL      'QLGSORT'
169100011228     C                   PARM                    QLGSCB
169200011228     C                   PARM                    NotUsed
169300011228     C                   PARM                    NotUsed
169400011228     C                   PARM                    SizeList
169500011228     C                   PARM                    ReturnSize
169600011228     C                   PARM                    QUSEC
169700011228      * Next step - Write records to I/O routine.
169800011228     C                   EVAL      QLGRT00 = Put
169900011228     C                   DO        RrnLast       x
170000030604     C     x             CHAIN     video2
170100011228     ** Solo le righe con Selected = 'Y' sono riordinate,
170200011228     ** quindi per fare un ordinamento di tutte le righe
170300011228     ** metto 'Y' sempre.
170400011228     C                   EVAL      Selected = 'Y'
170500011228     C                   CLEAR                   QUSEI
170600011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
170700011228     C                   CALL      'QLGSRTIO'
170800011228     C                   PARM                    QLGSCB00
170900011228     C                   PARM                    SflRcd
171000011228     C                   PARM                    NotUsed
171100011228     C                   PARM                    SizeList
171200011228     C                   PARM                    NotUsed
171300011228     C                   PARM                    QUSEC
171400011228     C                   ENDDO
171500011228      * Next step - Signal end of input, clear subfile for reload.
171600011228     C                   EVAL      QLGRT00 = EndPut
171700011228     C                   CLEAR                   QUSEI
171800011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
171900011228
172000011228     C                   CALL      'QLGSRTIO'
172100011228     C                   PARM                    QLGSCB00
172200011228     C                   PARM                    SflRcd
172300011228     C                   PARM                    NotUsed
172400011228     C                   PARM                    SizeList
172500011228     C                   PARM                    NotUsed
172600011228     C                   PARM                    QUSEC
172700011228     c                   seton                                        90
172800030604     C                   WRITE     video2c
172900011228     C                   setoff                                       90
173000011228      * Final step - Write the records back to the subfile.
173100011228     C                   EVAL      QLGRT00 = Get
173200011228     C                   DO        RrnLast       x
173300011228     C                   CLEAR                   QUSEI
173400011228     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
173500011228     C                   CALL      'QLGSRTIO'
173600011228     C                   PARM                    QLGSCB00
173700011228     C                   PARM                    NotUsed
173800011228     C                   PARM                    SflRcd
173900011228     C                   PARM                    QLGRL00
174000011228     C                   PARM                    NotUsed
174100011228     C                   PARM                    QUSEC
174200030605     c                   z-add     x             snrr1
174300030604     C                   WRITE     video2
174400011228     C                   ENDDO
174500011228     C                   ENDSR
174600090318      * ?___________________________________________________________________
174700000607     C     *INZSR        BEGSR
174800090318      * ?___________________________________________________________________
174900000607      *
175000000607     C     *ENTRY        PLIST
175100000607     C                   PARM                    Kpjba
175200080328      *
175300030604     C                   Z-ADD     1             CODUT
175400030604     C                   CALL      'X§PARUT'
175500030604     C                   PARM                    UTEDSE
175600030604     C                   MOVEL     REC80         CNCR80
175700110502     c                   eval      vtitolo = '*  Gestione  Autotrasportatori  *'
175800080328      *
175900080328      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
176000030617     C                   CLEAR                   Tibs36Ds
176100030617     C                   EVAL      I36ute = knmus
176200030617     C                   CALL      'TIBS36R'
176300030617     C                   PARM                    tibs36ds
176400080328      * profilo di filiale o di sede
176500030617     c                   if        o36pou <> 046
176600071113     c                   seton                                        2030
176700110114      *se manutenzione e profilo filiale proteggo selezione filiale ognuno vede i suoi
176800110114     c                   if        kcdaz = 'MZ70'
176900110114     c                   seton                                        19
177000110114     c                   endif
177100030617     c                   endif
177200080328      *
177300071113      * profilo REA libera per area idoneità professionale e capacità finanziaria
177400071113      * lascia protetti altri campi riservati alla sede
177500071113     c                   if        %subst(knmus:1:3) = 'REA'
177600071113     c                   setoff                                       30
177700080109     c* reperisco elenco P.O area da poter gestire
177800080109     c                   clear                   trul31ds
177900080109     c                   clear                   trul31ds2
178000080109     c                   move      'RA'          I31abi
178100080109     c                   z-add     o36pou        i31cpo
178200080109     c                   call      'TRUL31R'
178300080109     c                   parm                    kpjba
178400080109     c                   parm                    trul31ds
178500080109     c                   parm                    trul31ds2
178600080109     c
178700080109     c                   z-add     o36pou        v1cfil
178800071113     c                   endif
178900050615      * se il codice azione è diverso dall'originale MZ70 non mostro le
179000070619      * opzioni in seguito gestisco solo la visualizzazione
179100050615     c                   if        kcdaz <> 'MZ70'
179200110502     c                   eval      vtitolo = '* Interrogaz. Autotrasportatori *'
179300050615     c                   seton                                        2026
179400110114     c                   if        o36pou <> 046
179500110114     c                   z-add     o36pou        v1cfil
179600110114     c                   endif
179700050615     c                   endif
179800040610     C*-
179900040610     C*   Imposta la data del giorno in formato ISO
180000040610     C                   TIME                    W0140            14 0
180100040610     C                   MOVEl     W0140         Wora              6 0
180200040610     C                   MOVE      W0140         WDAT              8 0
180300040610     C                   Z-ADD     Wdat          G02DAT
180400040610     C                   MOVEL     *BLANK        G02ERR
180500040610     C                   CALL      'XSRDA8'
180600040610     C                   PARM                    WLBDA8
180700040610     C* UDATE A 8 IN AAAA/MM/GG
180800080811     C                   move      G02INV        wdatg             8 0
180900120103     C                   MOVE      wdatg         WDATalfa          8
181000040610     C                   move      G02INV        dataoggi
181100090317      * reperimento autorizzazioni per gestione società o campi riservati
181200090317     c                   clear                   daua
181300090317     c                   clear                   trmz77ds
181400090317     c                   movel     knmus         i77prf
181500090317     c                   call      'TRMZ77R'
181600090317     c                   parm                    trmz77ds
181700090317     c                   movel     o77uni        daua
181800160112     c                   setoff                                       0102
181900160112     c                   if        kcdaz <>'MX70'
182000160112     c                   if        §AUAGES = 'S'
182100160304     c                   seton                                        01
182200160304     c                   else
182300160304     c                   seton                                        20
182400160112     c                   endif
182500160112     c                   if        §AUAIPR = 'S'
182600160112     c                   seton                                        02
182700160112     c                   endif
182800160112     c                   endif
182900000607      *
183000030729     C     Kapd          KLIST
183100030729     C                   KFLD                    tipoa             1
183200081126     C                   KFLD                    vkcod             7 0
183300170221      *
183400170221     C     Ktbe          KLIST
183500170221     C                   KFLD                    tbecod
183600170221     C                   KFLD                    tbeke1
183700000607      *---------------------------------------------------------------------
183800080321      *
183900090316     ** Inizializzo il programma. per dati societari
184000090316     C                   CALL      'TIBSSOCR'
184100090316     C                   PARM      'INIT'        prmRqsOpCode
184200090316     C                   PARM                    prmRpyOpCode
184300090316     C                   PARM                    prmRpyIdMsg
184400090316
184500011227     C                   ENDSR
184600090318      * ?___________________________________________________________________
184700011227     c     srsql         begsr
184800090318      * ?___________________________________________________________________
184900011227     C                   EVAL      WrkSqlCmd
185000011227     C                             =
185100090317     c                             'SELECT RRN(aitra01l), aitra01l.* -
185200090317     C                             FROM AITRA01L'
185300011227     C                             +
185400030604     C                             ' WHERE TRATIP = ''C'''
185500101227     c*scelta raggruppamento
185600101227      *
185700030604     C                   IF        v1cfil <> 046 and v1cfil <> *zeros
185800011227     C                   EVAL      WrkSqlCmd
185900011227     C                             =
186000011227     C                             %TRIMR(WrkSqlCmd)
186100011227     C                             +
186200030604     C                             ' AND trafil = -
186300030605     C                              ' +  %editc(v1cfil:'Z')
186400011227     C                   endif
186500030617     c*disaccredtati si o no
186600030619     C                   IF        v1cdis = 'N'
186700030617     C                   EVAL      WrkSqlCmd
186800030617     C                             =
186900030617     C                             %TRIMR(WrkSqlCmd)
187000030617     C                             +
187100030619     C                             ' AND tradfi = 0 '
187200030617     C                   endif
187300081126     c*solo autisti città
187400081126     C                   IF        v1taut = 'C'
187500081126     C                   EVAL      WrkSqlCmd
187600081126     C                             =
187700081126     C                             %TRIMR(WrkSqlCmd)
187800081126     C                             +
187900081126     C                             ' AND trakau > 0 '
188000081126     C                   endif
188100090317     c*solo autisti aff/def
188200081126     C                   IF        v1taut = 'A'
188300081126     C                   EVAL      WrkSqlCmd
188400081126     C                             =
188500081126     C                             %TRIMR(WrkSqlCmd)
188600081126     C                             +
188700081126     C                             ' AND trabpt > 0 '
188800081126     C                   endif
188900030604     c*scelta nome autista
189000030604     C                   IF        v1caut <> '*'
189100011227     C                   EVAL      WrkSqlCmd
189200011227     C                             =
189300011227     C                             %TRIMR(WrkSqlCmd)
189400011227     C                             +
189500030604     C                             ' AND traaut like ' +
189600030605     C                             '''' +  %trimr(v1caut) + '%' + ''''
189700011227     C                   endif
189800030604     c*scelta società autista
189900030604     C                   IF        v1csoc <> '*'
190000030604     C                   EVAL      WrkSqlCmd
190100030604     C                             =
190200030604     C                             %TRIMR(WrkSqlCmd)
190300030604     C                             +
190400030604     C                             ' AND trasoc like ' +
190500030605     C                             ''''  + %trimr(v1csoc) + '%' + ''''
190600030604     C                   endif
190700030604     c*scelta nome autista
190800030604     C                   IF        v1ctaa <> '*'
190900030604     C                   EVAL      WrkSqlCmd
191000030604     C                             =
191100030604     C                             %TRIMR(WrkSqlCmd)
191200030604     C                             +
191300030604     C                             ' AND trataa like ' +
191400030604     C                             '''' + '%' + %trimr(v1ctaa) + '%' + ''''
191500030604     C                   endif
191600070523     c*scelta macchina di scorta
191700070523     C                   IF        v1csco <> '*'
191800070523     C                   EVAL      WrkSqlCmd
191900070523     C                             =
192000070523     C                             %TRIMR(WrkSqlCmd)
192100070523     C                             +
192200070523     C                             ' AND trasco like ' +
192300070523     C                             '''' + '%' + %trimr(v1csco) + '%' + ''''
192400070523     C                   endif
192500170221     c*scelta autista senza automezzo       (ex  autista di riserva)
192600070523     C                   IF        v1cris <> '*'
192700070523     C                   EVAL      WrkSqlCmd
192800070523     C                             =
192900070523     C                             %TRIMR(WrkSqlCmd)
193000070523     C                             +
193100170221     C                             ' AND traris <> '' ''  '
193200070523     C                   endif
193300100113     c*scelta autista a perdere
193400100113     C                   IF        v1ccor <> '*'
193500100113     C                   EVAL      WrkSqlCmd
193600100113     C                             =
193700100113     C                             %TRIMR(WrkSqlCmd)
193800100113     C                             +
193900100113     C                             ' AND tracor like ' +
194000100113     C                             '''' + '%' + %trimr(v1ccor) + '%' + ''''
194100100113     C                   endif
194200080327     c*scelta società appartenenza
194300030604     C                   IF        v1csoca <> '*'
194400011227     C                   EVAL      WrkSqlCmd
194500011227     C                             =
194600011227     C                             %TRIMR(WrkSqlCmd)
194700011227     C                             +
194800030604     C                             ' AND trasocg = ' + '''' + v1csoca + ''''
194900011227     C                   endif
195000081202     c*scelta codice autista
195100081202     C                   IF        v1ccod <> 9999999 and v1ccod <> *zeros
195200081202     c                             and v1taut = 'C'
195300081202     C                   EVAL      WrkSqlCmd
195400081202     C                             =
195500081202     C                             %TRIMR(WrkSqlCmd)
195600081202     C                             +
195700081202     C                             ' AND trakau = -
195800081202     C                              ' +  %editc(v1ccod:'Z')
195900081202     C                   endif
196000081202     C                   IF        v1ccod <> 9999999 and v1ccod <> *zeros
196100081202     c                             and v1taut = 'A'
196200081202     C                   EVAL      WrkSqlCmd
196300081202     C                             =
196400081202     C                             %TRIMR(WrkSqlCmd)
196500081202     C                             +
196600081202     C                             ' AND trabpt = -
196700081202     C                              ' +  %editc(v1ccod:'Z')
196800081202     C                   endif
196900030613     C                   EVAL      WrkSqlCmd
197000030613     C                             =
197100030613     C                             %TRIMR(WrkSqlCmd)
197200030613     C                             +
197300030613     C                             ' order by traaut '
197400011227     c*
197500011227     C                   ENDSR
197600080327     c*--------------------------------------------------------------
