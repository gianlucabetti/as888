000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200120807     h dftactgrp(*no) actgrp(*new)  BNDDIR('TIBS')
000300120807     H BNDDIR('TIBS') EXTBININT(*YES)
000400120628      *--------------------------------------------------------------*
000500030709     Fficn76d   CF   E             WORKSTN
000600030604     f                                     sfile(video2:snrr1)
000700030709     Ffifli01l  if   E           k disk
000800081119     Ffiadt01l  if   E           k disk
000900081119     Ffiadd02l  if   E           k disk
001000030709     Ffiatt01l  uf a E           k disk
001100130522     Ffiatt11l  uf a E           k disk
001200030710     Ffiatt02l  if   E           k disk    rename(fiatt000:fiatt2)
001300030709     Fazorg01l  if   E           k disk
001400030714     Ffiapd01l  if   E           k disk
001500040209     fAzcln01l  if   e           k Disk
001600090512     fAitrs01l  if   e           k Disk
001700090512     fAnfrn01l  if   e           k Disk
001800090513     fAitra05l  if   e           k Disk
001900090512     fAnsog01l  if   e           k Disk
002000050419     ftntbe01l  if   e           k disk
002100090511     ftabel00f  if   e           k disk
002200060120     ffnfvv08l  if   e           k disk    usropn
002300120628ab   ***************************************************************************
002400011228      *
002500020103     D Psds           SDS
002600020103     D  PgmName          *PROC
002700011228     D Kpjba         E DS
002800011228      *
002900030702     D fnlv55ds      E DS
003000030721     D fnlv24ds      E DS
003100030619     D trul33ds      E DS
003200130522     D ficn71ds      E DS
003300131007     D uteautds      E DS
003400131007     d tibs42ds      e ds
003500100617     d dvpocont      e ds
003600100617     d tibs02ds      e ds
003700030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003800030604     D CNCR80        E DS
003900030617     d Tibs36Ds      e ds
004000050419     d Dbld          e ds
004100090511     D DS5Aaut       E DS
004200120628     d ds3i          e ds
004300130507     d ficn5ads      e ds
004400011227     d data            ds            10
004500011228     d aa                      3      4
004600011228     d mm                      6      7
004700011228     d gg                      9     10
004800120705      *
004900120705     D errter          S              1
005000030702     D fatto           S              1
005100030610     D modalita        S              1
005200030711     D prime_time      S              1    inz('X')
005300030604     D x               S              4  0
005400011227     D WrkEofS01       S              1
005500011227     D WrkCarS01       S              1
005600011228     D $VIDEO          S             10
005700030604     D snrr1           S              5i 0
005800030721     D Savkpjbu        S                   like(kpjbu)
005900030604     D SavOpzione      S                   like(v1csce)
006000030709     D Savattter       S                   like(vattter)
006100030709     D Savattterd      S                   like(vattterd)
006200030606     d dataiso         s               d   datfmt(*iso)
006300030606     d dataeur         s               d   datfmt(*eur)
006400060120     d dataiso2        s               d   datfmt(*iso)
006500090512     d datavaliso      s               d   datfmt(*iso)
006600090512     d dataconiso      s               d   datfmt(*iso)
006700040209     d ktfp            s                   Like(clntfp)
006800040209     d ktfa            s                   Like(clntfa)
006900040209     d kann            s                   Like(ClnAnn)
007000040209     d kmes            s                   Like(ClnMes)
007100060120     d mese            s                   Like(ClnMes)
007200051027     d wnpg            s                   Like(fvvnpg)
007300051027     d wdfv            s                   Like(fvvdfv)
007400051027     d wfgs            s                   Like(fvvfgs)
007500090513     d data0           s              8  0
007600120619     d*
007700120619     D dsaiats       E DS                  EXTNAME(AIATS00F)
007800120619     d*
007900120619     D partraz         DS
008000120619     D  parsoctraz                    3s 0
008100120619     D  parivatraz                   16
008200120619     D  pardtintraz                   8s 0
008300120619     D  pardtfitraz                   8s 0
008400120619     d  esitotraz                     1
008500120619     d  errmsgtraz                   75
008600120619      *
008700090702     D PARAM           DS
008800090702     d  parterm                       3  0
008900090702     d  pardat                        8  0
009000090702     d  parpdr                        7  0
009100090702     d  paresito                      1
009200040209     d                 ds
009300040209     d  ClnPom                 1     31
009400040209     d  Pom                    1     31    Dim(31)
009500040209     d                 ds
009600040209     d  ClnMat                 1     31
009700040209     d  Mat                    1     31    Dim(31)
009800040209
009900030613     D WLBDA8          DS
010000030613     D  G02DAT                 1      8  0
010100030613     D  G02INV                 9     16  0
010200030613     D  G02ERR                17     17
010300030613     D  G02TGI                18     22  0
010400051027     D Cmd             S             48                                         QCAEXEC
010500051027     D Cmd1            S             48    dim(1) ctdata perrcd(1)              QCAEXEC
010600051027     D Cmd2            S             48    dim(1) ctdata perrcd(1)              QCAEXEC
010700120628ab   ***************************************************************************
010800120628     D socieTRAZ       S              3A
010900120628     D Filiale_Aut...
011000120628     d                 s              3s 0
011100120628ab   **
011200120807ab   ** Prototipi.
011300120807ab    **
011400120807ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
011500120807ab    /DEFINE DFTACTGRP_NO
011600120807ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
011700120807ab    /UNDEFINE DFTACTGRP_NO
011800120807      *
011900120807ab   D TibsSof_esito...
012000120807ab   D                 S             10I 0 IMPORT
012100120807ab   D idSocieta...
012200120807EDPDCD                 S              3A
012300120807ab   **
012400120807EDPDCd dtVALIDITA      s               D
012500120807EDPDCd VALdatFINE      s               D
012600120807EDPDCd VALdatINIZ      s               D
012700120807      *---------------- Gestione formato video video01----------------------
012800120807ab   C*     Inizializza
012900120807ab    /FREE
013000120807ab     TibsSof_Init();
013100120807ab    /END-FREE
013200030604     C                   EXSR      Inzvideo1
013300030604     C                   DOU       $Video <> 'VIDEO1'
013400000608      *----------- Visualizzo dati di output in caso di errori.-------------
013500000608     C                   IF        *IN99
013600000608     C                   EVAL      *IN99 = *OFF
013700030604     C                   WRITE     video1
013800000608     C                   EVAL      *IN99 = *ON
013900000608     C                   ENDIF
014000110915     c                   clear                   calen             1
014100000608      *
014200030702     c     emetti        tag
014300030604     C                   EXFMT     video1
014400000608      *
014500011227     C                   SELECT
014600030612     C                   WHEN      *INKC
014700011227     C                             OR
014800011227     C                             *INKL
014900011227     C                   EVAL      $Video = *BLANKS
015000011227
015100011227     C                   WHEN      *INKE
015200030604     C                   EXSR      Inzvideo1
015300011227     C                   WHEN      *INKB
015400030604     C                   EXSR      Chkvideo1
015500011227     C                   OTHER
015600030604     C                   EXSR      Chkvideo1
015700040209     c   45
015800051027     cor 46
015900040209     cor 43
016000030715     cor 44              goto      emetti
016100030604     C  N99              EXSR      Wrkvideo2c
016200031020     C     OK            CABEQ     0             EMETTI
016300011227     C                   ENDSL
016400011227      *
016500011227     C                   ENDDO
016600011227      *
016700011227     C                   EXSR      Uscita
016800011227     c**********************************************************************
016900011227     c* uscita
017000011227     c**********************************************************************
017100011227     C     Uscita        BEGSR
017200011227      *
017300011227     C                   EVAL      *INLR = *ON
017400011227     C                   RETURN
017500011227      *
017600011227     C                   ENDSR
017700011227     c**********************************************************************
017800011227     c* gestione sfl
017900011227     c**********************************************************************
018000030604     C     Wrkvideo2c    BEGSR
018100000627      *
018200011227     C                   EVAL      WrkCarS01 = *ON
018300030604     C                   EVAL      $Video = 'VIDEO2C'
018400011227      *
018500030604     C                   DOU       $Video <> 'VIDEO2C'
018600030604     C                   EXSR      Carvideo2
018700011228     c*Descrizione della ricerca
018800000627      *
018900031020     C     OK            CABEQ     0             TORNA
019000120706      *
019100030604     C                   WRITE     video2z
019200030604     C                   EXFMT     video2c
019300120706      *
019400011227     C                   SELECT
019500011228     c* fine
019600011227     C                   WHEN      *INKC=*ON
019700011227     C                   EVAL      $VIDEO=*BLANKS
019800011228     c* guida
019900011227     C                   WHEN      *INKL=*ON
020000030604     C                   EVAL      $VIDEO='VIDEO1'
020100011228     c* ripristino
020200011227     C                   WHEN      *INKE=*ON
020300011227     C                   EVAL      WrkCarS01 = *ON
020400030711     c                   move      *blank        prime_time
020500030715
020600030715     C                   WHEN      *INKa=*ON
020700030715     C                   exsr      spalmaopz
020800011227
020900011227     C                   OTHER
021000011227
021100011227     C                   EXSR      GestioneSFL
021200011227     C                   ENDSL
021300011228
021400011227     C                   ENDDO
021500031020     C     torna         ENDSR
021600011227     c**********************************************************************
021700011227     c* Controlli video R01.
021800011227     c**********************************************************************
021900030604     C     Chkvideo1     BEGSR
022000000609      *
022100000608     C                   EVAL      *IN99 = *OFF
022200030702     C                   EVAL      *IN44 = *OFF
022300030715     C                   EVAL      *IN43 = *OFF
022400040209     C                   EVAL      *IN45 = *OFF
022500051027     C                   EVAL      *IN46 = *OFF
022600160908     C                   EVAL      *IN28 = *OFF
022700120705      *
022800120705      *
022900060915      * profilo di filiale controllo che il terminal sia congruente
023000160908     c*m                 if        o36pou <> 046
023100160908     c*m                 if        vattter <> terma and vattter <> termp
023200160908     c*m                 seton                                        44
023300160908     c*m                 endif
023400160908     c*m                 else
023500030702     c
023600030702     c                   clear                   fnlv55ds
023700030702     c                   move      'L'           d55tla
023800030709     c                   move      vattter       d55lin
023900100617     c                   if        wuda >= §vpoter
024000100617     c                   move      'E'           d55tpt
024100100617     c                   else
024200100617     c                   move      ' '           d55tpt
024300100617     c                   endif
024400030702     c                   z-add     wuda          d55drf
024500030702     c                   call      'FNLV55R'
024600030702     c                   parm                    fnlv55ds
024700030709     c                   if        vattter <> d55tfp and vattter <> d55tfa
024800030709     c                             or vattter = *zeros
024900030702     c                   seton                                        44
025000030702     c                   endif
025100160908     c*m                 endif
025200030709     c     vattter       chain     azorg01l
025300030709     c                   movel     orgdes        vattterd
025400030709     c                   if        vdata1 > 0
025500030715     c                   move      vdata1        g02dat
025600030715     c                   move      *blank        g02err
025700030715     C                   CALL      'XSRDA8'
025800030715     C                   PARM                    WLBDA8
025900030715     C     G02ERR        IFEQ      '1'
026000030715     C                   SETON                                        43
026100030715     C                   END
026200130812      * controllo autorizzazione del profilo
026300130812     c                   if        wuda    >= §vpoaut
026400131007     c                   clear                   tibs42ds
026500131007     c                   movel     vattter       i42pge
026600131007     c                   call      'TIBS42R'
026700131007     c                   parm                    tibs42ds
026800131007     c                   movel     o42uni        uteautds
026900130812     c                   if        §AFFAGV  <>  'S'
027000130812     c                   seton                                        5799
027100130812     c                   leavesr
027200130812     c                   endif
027300160908     c                   endif
027400030715     c                   move      g02dat        vdata1
027500030715     c                   move      g02inv        wuda
027600030715     c                   move      g02inv        vdata1g
027700040209      *controllo della data sul calendario per verificare se è una data
027800040209      *utile per aprire i viaggi
027900110914     c                   clear                   ktfp
028000040209     c                   clear                   ktfa
028100040209     C                   MOVEL     vdata1g       KANN
028200040209     C                   move      vdata1g       w0040             4 0
028300040209     C                   movel     w0040         kmes
028400040209     C                   move      w0040         gi                2 0
028500040209     C     kazcln        chain     azcln01l                           30
028600110915    2C                   if        %found(azcln01l) and calen = *blank
028700051027    3c                   if        mat(gi) = 'F'
028800051027     c                             or pom(gi) = 'F'
028900040209     c*
029000040209     C                   SETON                                        45
029100110915     c                   move      'X'           calen
029200110914     c                   exsr      verfogli
029300060120     c                   else
029400060120      * verifico congruenze per aperture
029500060120     c                   exsr      verfogli
029600040209    3c                   endif
029700051027      *
029800040209    2c                   endif
029900040209      * Data uguale a 0
030000040209     c                   else
030100040209     c                   seton                                        43
030200030709     c                   endif
030300000608      *
030400000608     C                   ENDSR
030500060120      *-------------------------------------------------------------------
030600060120     c     verfogli      begsr
030700060120      *-------------------------------------------------------------------
030800060120     c                   setoff                                       4246
030900060120      * verifico la congruenza della data per apertura del foglio
031000060120      * se trovo un foglio arrivi nella stessa data posso aprire i fogli
031100060120      * se non trovo un foglio nella stessa data verifico l'ultimo foglio
031200060120      * aperto dalla filiale se è nello stesso mese posso aprire i fogli
031300060120      * se non è nello stesso mese non posso aprire i fogli in quella data
031400060120      * ma devo forzare l'apertura nel giorno del foglio del mese precedente
031500060120     c                   exsr      comando
031600060120     c                   open      fnfvv08l
031700060120     c                   z-add     2             wnpg
031800060120     c                   z-add     vdata1g       wdfv
031900060120     c                   move      vattter       wfgs
032000060120     c     kfvv          chain     fnfvv08l
032100060120     c                   if        not %found(fnfvv08l)
032200060120     c     kfvv2         setgt     fnfvv08l
032300060120     c     kfvv2         readpe    fnfvv08l
032400060120     c                   if        %eof(fnfvv08l)
032500060120     C                   seton                                        42
032600060120     c                   else
032700060120     c                   move      fvvdfv        dataiso2
032800060120     c                   extrct    dataiso2:*m   mese
032900060120     c     kmes          comp      mese                               4646
033000060120     c                   endif
033100060120     c                   endif
033200060120     c                   close     fnfvv08l
033300060120    3c                   endsr
033400011227     c**********************************************************************
033500011227     c* inizializza r01
033600011227     c**********************************************************************
033700030604     C     Inzvideo1     BEGSR
033800000608
033900030702     c                   if        o36pou <> 046
034000060915     c                   move      termp         vattter
034100030702     c                   endif
034200030709     c                   move      wdat          vdata1
034300030709     c                   move      wuda          vdata1g
034400030604     C                   EXSR      Chkvideo1
034500000609
034600000608     C                   ENDSR
034700030604     c**********************************************************************
034800030605     C     opzione       BEGSR
034900030605     c**********************************************************************
035000030610     c                   move      v1csce        modalita
035100030605     c                   select
035200050420      * inserimento da listino standard
035300030710     c                   when      v1csce = '1'
035400030710     c                   exsr      inserisce
035500050420      * modifica viaggi x giornata
035600130522     c                   when      v1csce = '2' or
035700130522     c                             v1csce = 'P'
035800081120     c                   if        vattdcoh > 0
035900081120     c                   seton                                        2128
036000090505     c                   eval      $msg = 'Viaggio già confermato non -
036100081120     c                             modificabile'
036200081120     c                   endif
036300030605     c                   exsr      modifica
036400050420     c                   setoff                                       2128
036500050420     c                   clear                   $msg
036600050420      * copia viaggi x giornata
036700030605     c                   when      v1csce = '3'
036800030605     c                   exsr      copia
036900050420     c                   setoff                                       2128
037000050420     c                   clear                   $msg
037100050420      * visualizza
037200030605     c                   when      v1csce = '5'
037300030606     c                   seton                                        21
037400030606     c                   exsr      modifica
037500030606     c                   setoff                                       21
037600050420      * cancella
037700081121     c                   when      v1csce = '4'
037800081121     c                   seton                                        21
037900081121     c                   if        vattdcoh > 0
038000081121     c                   seton                                        28
038100090505     c                   eval      $msg = 'Viaggio già confermato non -
038200081121     c                             cancellabile'
038300081121     c                   endif
038400030825     c                   exsr      modifica
038500081121     c                   setoff                                       2128
038600030825     c                   clear                   $msg
038700030605     c                   endsl
038800030604     C                   ENDSR
038900030710     c**********************************************************************
039000030710     C     inserisce     BEGSR
039100030710     c**********************************************************************
039200030710     c     katt2         chain     fiatt02l                           98
039300030710     c                   if        *in98
039400030710     c                   clear                   fiatt000
039500030710     c*                  exsr      aggiorna
039600030710     c                   move      vattVAD       attVAD
039700030710     c                   move      vattTER       attTER
039800030710     c                   move      vattFGP       attFGP
039900030710     c                   move      vattFGA       attFGA
040000130522     c                   move      vdata1g       attDDc
040100130522     c                   move      attddc        dataiso
040200130522     c                   move      dataiso       dataeur
040300130522     c                   move      dataeur       vattddc
040400030710     c                   move      vattORP       attORP
040500030710     c                   move      vattORA       attORA
040600030710     c                   move      vattPRV       attPRV
040700081120     c*mm                z-add     vattIMP2      attIMP
040800030721     c                   move      *zeros        attPDR
040900030710     c                   move      vattPDR       attPDR
041000031203     c                   move      vattnot       attnot
041100031103     c                   if        vattpdr <> *zeros and vattpdr <> *blank
041200081120     c                   move      vattpdr       vattpdrk
041300081120     c                   move      vattpdr       vattpdrh
041400081120     c                   exsr      repimporto
041500081120     c                   z-add     importo       attimp
041600090717     c                   eval      attcdf = ADTCDFs
041700090717     c                   eval      attsoc = ADTCSFs
041800031103     c                   else
041900031103     c                   clear                   attcdf
042000031103     c                   clear                   attsoc
042100031103     c                   endif
042200030710     c                   exsr      repnum
042300130521     c* piombi con barcode
042400130522     c                   clear                   vattpmb
042500130524     c*                  exsr      reppmb
042600130524     c*                  movel(p)  iO71Pmb       attpmb
042700130524     c*                  movel(p)  iO71Pmb       vattpmb
042800130522     c* se ci sono più di 20 byte di piombi scrivo estensione FIATT10F
042900130524     c*                  if        %subst(vattPmb: 21: 30) <> ' '
043000130524     c*                  exsr      srwrt10
043100130524     c*                  end
043200030710     c                   write     fiatt000
043300030710     c                   endif
043400030710     C                   ENDSR
043500030605     c**********************************************************************
043600130522     C     srwrt10       BEGSR
043700030605     c**********************************************************************
043800130522     c                   clear                   fiatt100
043900130522     c                   move      attVAD        at1vad
044000130522     c                   move      attTER        at1ter
044100130522     c                   move      attFGP        at1fgp
044200130522     c                   move      attFGA        at1fga
044300130522     c                   move      attddc        at1ddc
044400130522     c                   move      attprg        at1prg
044500130522     c                   eval      at1pmb = %subst(vattPmb: 21: 30)
044600130522     c                   write     fiatt100
044700130522     c                   endsr
044800130522     c**********************************************************************
044900130522     C     modifica      BEGSR
045000130522     c**********************************************************************
045100030710     c     katt1         chain     fiatt01l                           98
045200030610     c                   if        not *in98
045300050420      * se il foglio ha la data fatturazione impedisco la modifica
045400050420     c                   if        modalita ='2'and
045500050420     c                             attdft > 0
045600050420     c                   seton                                        21
045700050420     c                   endif
045800050420      *
045900030709     c                   move      attVAD        vattVAD
046000030709     c                   move      attTER        vattTER
046100030709     c                   move      attFGP        vattFGP
046200030709     c                   move      attFGA        vattFGA
046300130522      *data decorrenza
046400130522     c                   if        attddc > 0
046500130522     c                   move      attddc        dataiso
046600130522     c                   move      dataiso       dataeur
046700130522     c                   move      dataeur       vattddc
046800130522     c                   else
046900130522     c                   clear                   vattddc
047000130522     c                   endif
047100030709     c                   move      attORP        vattORP
047200030709     c                   move      attORA        vattORA
047300030709     c                   move      attPRV        vattPRV
047400030728     c                   move      attPRg        vattPRg
047500030709     c                   z-add     attIMP        vattIMP
047600030709     c                   z-add     attIMPp       vattIMPp
047700030709     c                   move      attPDR        vattPDR
047800030721     c                   move      attPDR        vattPDRk
047900081120     c                   move      attPDR        vattPDRh
048000030714     c     kapd          chain     fiapd01l                           95
048100090507     c  n95              movel     apdrsf        vattpdrd
048200030714     c   95              clear                   vattpdrd
048300030709     c                   move      attNOT        vattNOT
048400130522     c                   movel(p)  attpmb        vattpmb
048500130522     c     katt1         chain     fiatt11l
048600130522     c                   if        %found(fiatt11l)
048700130522     c                   move      at1pmb        vattpmb
048800130522     c                   end
048900130522     c                   if        modalita = 'P' and not *in21
049000130522     c                   exsr      reppmb
049100130522     c                   movel     iO71Pmb       Vattpmb
049200130522     c                   end
049300030701      * decodifica p.o.
049400030709     c     attfgp        chain     azorg01l                           95
049500030709     c  n95              movel     orgdes        vattfgpd
049600030709     c   95              clear                   vattfgpd
049700030709     c     attfga        chain     azorg01l                           95
049800030709     c  n95              movel     orgdes        vattfgad
049900030709     c   95              clear                   vattfgad
050000081120      *reperisce importo
050100081120     c                   if        attimp = 0 and attpdr > 0
050200081120     c                   exsr      repimporto
050300081120     c                   z-add     importo       vattimp
050400081120     c                   endif
050500030605     c                   exsr      Gesvideo3
050600030605     c                   endif
050700030605     C                   ENDSR
050800030605     c**********************************************************************
050900030605     C     copia         BEGSR
051000030605     c**********************************************************************
051100030710     c     katt1         chain     fiatt01l                           98
051200030701     c                   if        not *in98
051300050420      * se il foglio da copiare ha la data <= alla data protocollo errore
051400050420     c                   if        attddc <= §blddtp
051500050420     c                   z-add     attddc        wattddc           8 0
051600050420     c                   seton                                        21
051700050420     c                   else
051800050420     c                   clear                   wattddc
051900050420     c                   endif
052000050420      *
052100030709     c                   move      attVAD        vattVAD
052200030709     c                   move      attTER        vattTER
052300030709     c                   move      attFGP        vattFGP
052400030709     c                   move      attFGA        vattFGA
052500130522      *data decorrenza
052600130522     c                   if        attddc > 0
052700130522     c                   move      attddc        dataiso
052800130522     c                   move      dataiso       dataeur
052900130522     c                   move      dataeur       vattddc
053000130522     c                   else
053100130522     c                   clear                   vattddc
053200130522     c                   endif
053300030709     c                   move      attORP        vattORP
053400030709     c                   move      attORA        vattORA
053500030709     c                   move      attPRV        vattPRV
053600030709     c                   z-add     attIMP        vattIMP
053700030709     c                   z-add     attIMPp       vattIMPp
053800030709     c                   move      attPDR        vattPDR
053900030721     c                   move      attPDR        vattPDRk
054000081120     c                   move      attPDR        vattPDRh
054100030714     c     kapd          chain     fiapd01l                           95
054200090507     c  n95              movel     apdrsf        vattpdrd
054300030714     c   95              clear                   vattpdrd
054400030709     c                   move      attNOT        vattNOT
054500130522     c                   movel(p)  attpmb        vattpmb
054600130522     c     katt1         chain     fiatt11l
054700130522     c                   if        %found(fiatt11l)
054800130522     c                   move      at1pmb        vattpmb
054900130522     c                   end
055000030701      * decodifica p.o.
055100030709     c     attfgp        chain     azorg01l                           95
055200030709     c  n95              movel     orgdes        vattfgpd
055300030709     c   95              clear                   vattfgpd
055400030709     c     attfga        chain     azorg01l                           95
055500030709     c  n95              movel     orgdes        vattfgad
055600030709     c   95              clear                   vattfgad
055700030709     c                   clear                   fiatt000
055800030610     c                   exsr      Gesvideo3
055900030701     c                   endif
056000030605     C                   ENDSR
056100030605     c**********************************************************************
056200030605     C     gesvideo3     BEGSR
056300030605     c**********************************************************************
056400030702     c                   clear                   fatto
056500030605     c                   do        *hival
056600030605     c                   exfmt     video3
056700030605     c   kl              leave
056800030624     c                   exsr      controv3
056900130522     c* piombi
057000130522     c                   if        *inkm
057100130522     c                   exsr      reppmb
057200130522     c                   movel     iO71Pmb       Vattpmb
057300130522     c                   end
057400130522     c*
057500030624     c   96              iter
057600030605     c   kf              exsr      aggiorna
057700130522     c*
057800030702     c                   if        fatto <> *blank
057900030702     c                   leave
058000030702     c                   endif
058100030605
058200030605     c                   enddo
058300030605     C                   ENDSR
058400030605     c**********************************************************************
058500130522     C     reppmb        BEGSR
058600030605     c**********************************************************************
058700130522     c                   clear                   ficn71ds
058800130522     c                   eval      i71ter=vattter
058900130522     c                   eval      i71vad=vattvad
059000130522     c                   eval      i71fgp=vattfgp
059100130522     c                   eval      i71fga=vattfga
059200130522     c                   eval      i71prg=vattprg
059300130522     c                   move      vattddc       dataeur
059400130522     c                   move      dataeur       dataiso
059500130522     c                   move      dataiso       i71ddc
059600130522     c                   if        vattpdr <> ' '
059700130522     c                   movel     vattpdr       i71pdr
059800130522     c                   end
059900130522     c                   eval      I71ORP=vattorp
060000130522     c                   eval      I71ORA=vattora
060100130522     c                   eval      io71pmb=vattpmb
060200130522     c                   movel     ficn71ds      kpjbu
060300130522     c                   call      'FICN71R'
060400130522     c                   parm                    kpjba
060500130522     c                   movel     kpjbu         ficn71ds
060600130522     C                   ENDSR
060700130522     c**********************************************************************
060800130522     C     Controv3      BEGSR
060900130522     c**********************************************************************
061000030613      *
061100030702      * *in96 errore generico videata dettaglio
061200030825     c                   setoff                                       9628
061300030825     c                   clear                   $msg
061400030825      * annullamento controllo se foglio non stampato
061500030825     c                   if        modalita ='4'
061600030825     c                   if        attdst > 0
061700030825     c                   seton                                        9628
061800030825     c                   eval      $msg = 'Foglio non cancellabile già stampato'
061900030825     c                   goto      endctr
062000030825     c                   else
062100030825     c                   goto      endctr
062200030825     c                   endif
062300030825     c                   endif
062400050419      * modifica  controllo se foglio non fatturato
062500050419     c                   if        modalita ='2'
062600050419     c                   if        attdft > 0
062700050419     c                   seton                                        9628
062800050419     c                   eval      $msg = 'Foglio non modificabile fatturato'
062900050419     c                   goto      endctr
063000050419     c                   endif
063100050419     c                   endif
063200050419      * modifica  controllo se foglio non fatturato
063300050419     c                   if        modalita ='3'
063400050420     c                   if        wattddc < §blddtp and wattddc > 0
063500050419     c                   seton                                        9628
063600050419     c                   eval      $msg = 'Foglio non copiabile data minore -
063700050419     c                             alla data protocollo'
063800050419     c                   goto      endctr
063900050419     c                   endif
064000050419     c                   endif
064100030703      * decodifica p.o.
064200030709     c     vattfgp       chain     azorg01l                           95
064300030709     c  n95              movel     orgdes        vattfgpd
064400030709     c   95              clear                   vattfgpd
064500030709     c     vattfga       chain     azorg01l                           95
064600030709     c  n95              movel     orgdes        vattfgad
064700030709     c   95              clear                   vattfgad
064800030701      *controlli data  decorrenza
064900030613     c                   setoff                                       50
065000030709     c                   if        vattddc > 0
065100030709     C                   MOVE      Vattddc       G02DAT
065200030613     C                   MOVEL     *BLANK        G02ERR
065300030613     C                   CALL      'XSRDA8'
065400030613     C                   PARM                    WLBDA8
065500030613     C     G02ERR        IFEQ      '1'
065600030701     C                   SETON                                        5096
065700030613     C                   END
065800030709     c                   move      g02dat        vattddc
065900030709     c                   move      g02inv        vattddcg          8 0
066000030709     c                   endif
066100030714      * controllo autista
066200030714     c                   setoff                                       4847
066300030721     c                   if        vattpdr =  *zeros or vattpdr = *blank
066400030714     c                   seton                                        4896
066500030721     c                   goto      endctr
066600030714     c                   endif
066700030721     C****  CODICE PADRONCINO  ****
066800030721     C     '?'           SCAN      vattpdr                                55
066900030721     C*
067000030721     C     *IN55         IFEQ      *ON
067100030721     C                   MOVEL     KPJBU         SAVKPJBU
067200030721     C                   Z-ADD     Vattter       d24FIL
067300030721     C                   Z-ADD     0             d24pdr
067400030721     C                   MOVEL     'R'           d24FLG
067500030721     C                   MOVEL     'D'           d24tip
067600030721     C                   MOVEL     *BLANKS       d24rsc
067700030721     C                   MOVEL(p)  fnlv24ds      KPJBU
067800030721     C                   CALL      'FNLV24R'
067900030721     C                   PARM                    KPJBA
068000030721     C                   MOVEL     KPJBU         fnlv24ds
068100030721     C                   MOVEL     savKPJBU      KPJBU
068200030721     C*
068300030721     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
068400030721     C     d24pdr        IFNE      0
068500030721     C                   MOVEL     *ZEROS        Vattpdr
068600030721     C                   MOVEL     d24pdr        Vattpdr
068700030721     C                   MOVEL     vattpdr       Vattpdrk
068800030721     C                   MOVEL     d24rsc        vattpdrd
068900031027     c                   else
069000031027     C                   clear                   Vattpdr
069100031027     C                   clear                   Vattpdrk
069200031027     C                   clear                   vattpdrd
069300031027     c                   seton                                        4896
069400031027     c                   goto      endctr
069500030721     C                   ENDIF
069600030721     C*
069700030721     C                   ENDIF
069800030714     c                   movel     vattpdr       como3             3 0
069900030721     C                   MOVEL     vattpdr       Vattpdrk
070000030714     c                   if        como3 <> vattter
070100030714     c                   seton                                        4796
070200030721     c                   goto      endctr
070300030714     c                   else
070400030714     c     kapd          chain     fiapd01l                           95
070500080206     c                   if        not *in95 and apdksc <> 0
070600080207     c                             and apdatb = *blank
070700090507     c                   movel     apdrsf        vattpdrd
070800080206     c                   else
070900080206     c                   clear                   vattpdrd
071000080206     c                   seton                                        4796
071100080206     c                   endif
071200030714     c                   endif
071300081120      *reperisce importo
071400090630     c*                  if        vattimp = 0 and vattpdrk > 0 or
071500090630     c*                            vattpdrk <> vattpdrh
071600081120     c                   exsr      repimporto
071700081120     c                   z-add     importo       vattimp
071800090630     c*                  z-add     importo       vattimpp
071900090630     c*                  endif
072000081120      *Importo obbligatorio
072100090630     c                   if        vattimpp =  *zeros or
072200090630     c                             vattpdrk <> vattpdrh
072300081121     c                   z-add     vattimp       vattimpp
072400090630     c                   z-add     vattpdrk      vattpdrh
072500081120     c                   endif
072600030714      *controllo ore
072700030714     c                   setoff                                       52
072800130807     c                   if        vattorp < 2200
072900030714     c                   if        vattorp > vattora
073000030714     c                   seton                                        5296
073100030721     c                   goto      endctr
073200130806     c                   endif
073300030714     c                   endif
073400090508      *se importi diversi obbligo inserimento nota
073500090508     c                   setoff                                       41
073600090508     c                   if        vattimpp <> vattimp and vattnot = *blank
073700090508     c                   seton                                        4196
073800090508     c                   endif
073900030721     C     endctr        ENDSR
074000030605     c**********************************************************************
074100030605     C     aggiorna      BEGSR
074200030605     c**********************************************************************
074300030709     c                   move      vattVAD       attVAD
074400030709     c                   move      vattTER       attTER
074500030709     c                   move      vattFGP       attFGP
074600030709     c                   move      vattFGA       attFGA
074700030709     c                   move      vattORP       attORP
074800030709     c                   move      vattORA       attORA
074900030709     c                   move      vattPRV       attPRV
075000030709     c                   z-add     vattIMP       attIMP
075100030709     c                   z-add     vattIMPp      attIMPp
075200030714     c                   z-add     vattIMPp      vattIMPp2
075300030721     c                   if        vattpdr <> *zeros and vattpdr <> *blank
075400030721     c                   move      vattPDR       attPDR
075500090717     c                   eval      attcdf = ADTCDFs
075600090717     c                   eval      attsoc = ADTCSFs
075700030721     c                   else
075800030721     c                   clear                   attPDR
075900031103     c                   clear                   attcdf
076000031103     c                   clear                   attsoc
076100030721     c                   endif
076200030709     c                   move      vattNOT       attNOT
076300130522     c                   movel     vattpmb       attpmb
076400030709     c                   if        vattddc > 0
076500030709     c                   move      vattddc       dataeur
076600030610     c                   move      dataeur       dataiso
076700030709     c                   move      dataiso       attddc
076800030610     c                   endif
076900030610      * inserimento e copia scrive altrimenti aggiorna il record esistente
077000130522     c                   select
077100130522     c* CANCELLA
077200130522     c                   when      modalita = '4'
077300130521     c* controllo se presente file piombi
077400130522     c     katt1         chain     fiatt11l
077500130521     c                   if        %found(fiatt11l)
077600130521     c                   delete    fiatt100
077700130521     c                   end
077800130522     c                   delete    fiatt000
077900130522     c* COPIA
078000130522     c                   when      modalita = '3'
078100030701     c                   exsr      repnum
078200130522     c                   if        %subst(vattpmb: 21: 30) <> ' '
078300130522     c                   exsr      srwrt10
078400130522     c                   end
078500130522     c                   write     fiatt000
078600130522     c                   other
078700130522     c* AGGIORNA
078800130522     c     katt1         chain     fiatt11l
078900130522     c                   if        %found(fiatt11l)
079000130522     c                   if        %subst(vattpmb: 21: 30) <> ' '
079100130522     c                   eval      at1pmb = %subst(vattpmb: 21: 30)
079200130522     c                   update    fiatt100
079300130522     c                   else
079400130522     c                   delete    fiatt100
079500130522     c                   end
079600130522     c                   else
079700130522     c                   if        %subst(vattpmb: 21: 30) <> ' '
079800130522     c                   exsr      srwrt10
079900030610     c                   endif
080000130522     c                   endif
080100130522     c                   update    fiatt000
080200130522     c                   endsl
080300030702     c                   move      'X'           fatto
080400030605     C                   ENDSR
080500030619     c**********************************************************************
080600030728     c* reperimento numero viaggio
080700030619     c**********************************************************************
080800030619     C     repnum        BEGSR
080900030703     c                   clear                   trul33ds
081000030703     c                   move      'L'           i33tla
081100030709     c                   z-add     34            i33cnu
081200030703     c                   z-add     1             i33num
081300030703     c                   movel     trul33ds      kpjbu
081400030703     c                   call      'TRUL33R'
081500030703     c                   PARM                    kpjba
081600030703     c                   movel     kpjbu         trul33ds
081700030709     c                   z-add     o33nrf        attprg
081800030728     c                   z-add     o33nrf        vattprg
081900030619     C                   ENDSR
082000030715     c**********************************************************************
082100030715     C     spalmaopz     BEGSR
082200030715     c**********************************************************************
082300030715     c                   do        *hival        recor             4 0
082400030715     c     recor         chain     video2                             93
082500030715     c   93              leave
082600030715     c                   move      '1'           v1csce
082700030715     c                   update    video2
082800030715     c                   enddo
082900030715     C                   ENDSR
083000011228     c**********************************************************************
083100011228     c* carica sfl
083200011228     c**********************************************************************
083300030604     C     Carvideo2     BEGSR
083400011228     C                   IF        WrkCarS01 = *ON
083500030604     C                   EVAL      snrr1   = 0
083600011228     C                   EVAL      *IN90 = *ON
083700020219     C                   EVAL      *IN91 = *OFF
083800030604     C                   WRITE     video2c
083900011228     C                   EVAL      *IN90 = *OFF
084000030711     c                   setoff                                       2324
084100030711     c     kattd         setll     fiatt01l                               24
084200030711     c                   if        prime_time = *blank or not *in24
084300030711     c                   seton                                        23
084400030711     c                   exsr      listini
084500030711     c                   move      'X'           prime_time
084600030709     c                   else
084700030711     c     kattd         setll     fiatt01l
084800030701     c                   do        *hival
084900030711     c     kattd         reade     fiatt01l                               97
085000030701     c   97              leave
085100030701     c                   move      *blanks       v1csce
085200030709     c                   move      attVAD        vattVAD
085300030709     c                   move      attTER        vattTER
085400030709     c     attter        chain     azorg01l                           95
085500030709     c  n95              movel     orgdes        vattterd
085600030709     c   95              clear                   vattterd
085700030709     c                   move      attFGP        vattFGP
085800030709     c     attfgp        chain     azorg01l                           95
085900030709     c  n95              movel     orgdes        vattfgpd2
086000030709     c   95              clear                   vattfgpd2
086100030709     c                   move      attFGA        vattFGA
086200030709     c     attfga        chain     azorg01l                           95
086300030709     c  n95              movel     orgdes        vattfgad2
086400030709     c   95              clear                   vattfgad2
086500030709     c                   move      attORP        vattORP
086600030709     c                   move      attORA        vattORA
086700030709     c                   move      attPRV        vattPRV
086800030710     c                   move      attPRg        vattPRg
086900030721     c                   if        attpdr > 0
087000030709     c                   move      attPDR        vattPDR
087100030721     c                   else
087200030721     c                   clear                   vattPDR
087300030721     c                   endif
087400030709     c                   z-add     attimp        vattimp2
087500030709     c                   z-add     attimpp       vattimpp2
087600081120     c                   z-add     attdco        vattdcoh
087700030709      *data prestazione
087800030709     c                   if        attddc > 0
087900030709     c                   move      attddc        dataiso
088000030701     c                   move      dataiso       dataeur
088100030709     c                   move      dataeur       vattddc
088200030701     c                   else
088300030709     c                   clear                   vattddc
088400030701     c                   endif
088500030701      *esco per raggiunta massima capacità sfl
088600030617     c                   if        snrr1 > 9990
088700030617     c                   leave
088800030617     c                   endif
088900030617
089000030604     C                   EVAL      snrr1 = snrr1 + 1
089100030604     C                   WRITE     video2
089200011228     C                   ENDDO
089300011228
089400030604     C                   IF        snrr1  > 0
089500020219     C                   EVAL      *IN91 = *ON
089600020219     C                   MOVE      1             OK                1 0
089700020219     C                   ELSE
089800020219     C                   MOVE      0             OK                1 0
089900030701     C                   ENDIF
090000030612     C                   EVAL      nrr1  = 1
090100011228     C                   ENDIF
090200020219     C                   EVAL      WrkCarS01 = *OFF
090300030604     C                   EVAL      snrr1  = 1
090400030604     C                   EVAL      csrrrn = 1
090500030709      *prime_time
090600030709     c                   endif
090700011228     C                   ENDSR
090800030709     c**********************************************************************
090900030709     c* LISTINI   prime_time la prima volta carica dai listini
091000030709     c**********************************************************************
091100030709     c     listini       begsr
091200030709
091300030709     c     katt          setll     fifli01l
091400030709     c                   do        *hival
091500030709     c     katt          reade     fifli01l                               97
091600030709     c   97              leave
091700030709     c                   if        vdata1g >= flidde and vdata1g <= flidsc
091800030714      *controllo terminal se congruente nella data con azcae
091900130522     c* AFFLUENZA
092000030714     c                   if        flivad = 'A'
092100030714     c                   clear                   fnlv55ds
092200030714     c                   clear                   errter
092300030714     c                   move      'L'           d55tla
092400030714     c                   move      flifgp        d55lin
092500030714     c                   move      'P'           d55tpt
092600030714     c                   z-add     vdata1g       d55drf
092700030714     c                   call      'FNLV55R'
092800030714     c                   parm                    fnlv55ds
092900030714      *verifico se il terminal valido corrisponde a quello del listino
093000030714     c                   if        d55tfp <> fliter
093100030714     c                   move      'X'           errter
093200030714     c                   endif
093300030714     c                   else
093400130522     c* DEFLUENZA
093500030714     c                   clear                   fnlv55ds
093600030714     c                   clear                   errter
093700030714     c                   move      'L'           d55tla
093800030714     c                   move      flifga        d55lin
093900130522     c                   move      o36pou        D55LNP
094000100617     c                   if        vdata1g >= §vpoter
094100100617     c                   move      'D'           d55tpt
094200100617     c                   else
094300100617     c                   move      'A'           d55tpt
094400100617     c                   endif
094500030714     c                   z-add     vdata1g       d55drf
094600030714     c                   call      'FNLV55R'
094700030714     c                   parm                    fnlv55ds
094800030714      *verifico se il terminal valido corrisponde a quello del listino
094900030714     c                   if        d55tfa <> fliter
095000030714     c                   move      'X'           errter
095100030714     c                   endif
095200030714     c                   endif
095300030714      *se il terminal nella data non è valido scarto il record
095400030714     c                   if        d55err <> *blank or errter <> *blank
095500030714     c                   iter
095600030714     c                   endif
095700030714      *
095800030709     c                   move      *blanks       v1csce
095900030709     c                   move      FLIVAD        vattVAD
096000030709     c                   move      FLITER        vattTER
096100030709     c     fliter        chain     azorg01l                           95
096200030709     c  n95              movel     orgdes        vattterd
096300030709     c   95              clear                   vattterd
096400030709     c                   move      FLIFGP        vattFGP
096500030709     c     flifgp        chain     azorg01l                           95
096600030709     c  n95              movel     orgdes        vattfgpd2
096700030709     c   95              clear                   vattfgpd2
096800030709     c                   move      FLIFGA        vattFGA
096900030709     c     flifga        chain     azorg01l                           95
097000030709     c  n95              movel     orgdes        vattfgad2
097100030709     c   95              clear                   vattfgad2
097200030709     c                   move      FLIORP        vattORP
097300030709     c                   move      FLIORA        vattORA
097400030709     c                   move      FLIPRV        vattPRV
097500030721     c                   if        flipdr  > 0
097600030709     c                   move      FLIPDR        vattPDR
097700081119     c                   move      FLIPDR        vattPDRk
097800090506      *per caricamento listini non reperiamo importo viene controllato solo se si
097900090506      * conferma l'autista impostato
098000090506     c**                 exsr      repimporto
098100090506     c**                 z-add     importo       vattimp2
098200090506     c                   clear                   vattimp2
098300030721     c                   else
098400030721     c                   clear                   vattPDR
098500090506     c                   clear                   vattimp2
098600030721     c                   end
098700090506     c                   clear                   vattimpp2
098800081119     c**                 z-add     FLIimp        vattimp2
098900031203     c                   movel     FLInot        vattnot
099000030709      *esco per raggiunta massima capacità sfl
099100030709     c                   if        snrr1 > 9990
099200030709     c                   leave
099300030709     c                   endif
099400030709
099500030709     C                   EVAL      snrr1 = snrr1 + 1
099600030709     C                   WRITE     video2
099700030709     c                   endif
099800030709     C                   ENDDO
099900030709
100000030709     C                   IF        snrr1  > 0
100100030709     C                   EVAL      *IN91 = *ON
100200030709     C                   MOVE      1             OK                1 0
100300030709     C                   ELSE
100400030709     C                   MOVE      0             OK                1 0
100500030709     C                   ENDIF
100600030709     C                   EVAL      nrr1  = 1
100700030709     C                   EVAL      WrkCarS01 = *OFF
100800030709     C                   EVAL      snrr1  = 1
100900030709     C                   EVAL      csrrrn = 1
101000030709
101100030709     C                   ENDSR
101200011227     c**********************************************************************
101300011227     c* gestione sfl
101400011227     c**********************************************************************
101500000613     C     GestioneSFL   BEGSR
101600000627      *
101700000627     C                   EVAL      WrkEofS01 = *OFF
101800030605      * Elaborazione opzioni.
101900011228     c*
102000030605     c                   do        *hival        X                 4 0
102100030605     c     X             CHAIN     video2                             50
102200030605     C   50              LEAVE
102300030604     C                   IF        v1csce <> *blanks
102400030605     c                   exsr      opzione
102500030611
102600030611     c                   move      *blank        v1csce
102700030605     c                   update    video2
102800030611
102900011228     C                   ENDIF
103000030605     C                   enddo
103100030717     c                   if        modalita = '1' or modalita = '3'
103200050419     c                             or modalita = '4' or modalita = '2'
103300030710     c                   eval      wrkcars01 = *on
103400030710     c                   endif
103500000627      *
103600030703     C                   ENDSR
103700120807ab   C**************************************************************************
103800120807ab   C     TEST_SOCieta  begSR
103900120807ab    *
104000120807ab    /FREE
104100120807ab
104200120807EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( vattter
104300120807EDPDC                                             : dtVALIDITA
104400120807EDPDC                                             : 'O'
104500120807EDPDC                                             : valDatIniz
104600120807EDPDC                                             : valDatFine
104700120807ab                                                );
104800120807ab
104900120807EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
105000120807EDPDC    // Gestire l'errore.
105100120807ab     ENDIF;
105200120807ab
105300120807ab    /END-FREE
105400120807     c                   move      valdatfine    valdatfineW       8 0
105500120807ab   C                   ENDSR
105600011227     c**********************************************************************
105700000607     C     *INZSR        BEGSR
105800011227     c**********************************************************************
105900000607      *
106000000607     C     *ENTRY        PLIST
106100000607     C                   PARM                    Kpjba
106200030604     C                   Z-ADD     1             CODUT
106300030604     C                   CALL      'X§PARUT'
106400030604     C                   PARM                    UTEDSE
106500030604     C                   MOVEL     REC80         CNCR80
106600030703     C                   MOVEL     Ragut         rsut
106700030703     C                   MOVEL     knsif         vknsif
106800030703     C                   MOVEL     knmus         vknmus
106900030702      *reperimento data
107000030702     C                   TIME                    W0120            14 0
107100030702     C                   MOVE      W0120         WDAT              8 0
107200030702     C*
107300030702     C                   Z-ADD     WDAT          G02DAT
107400030702     C                   MOVEL     *BLANK        G02ERR
107500030702     C                   CALL      'XSRDA8'
107600030702     C                   PARM                    WLBDA8
107700030702     C* UDATE A 8 IN AAAA/MM/GG
107800030702     C                   Z-ADD     G02INV        WUDA              8 0
107900030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
108000030617     C                   CLEAR                   Tibs36Ds
108100030617     C                   EVAL      I36ute = knmus
108200030617     C                   CALL      'TIBS36R'
108300030617     C                   PARM                    tibs36ds
108400100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
108500100617     c                   clear                   tibs02ds
108600100617     c                   clear                   dvpocont
108700100617     c                   eval      t02mod = 'C'
108800100617     c                   eval      t02sif = knsif
108900100617     c                   eval      t02cod = 'VPO'
109000100617     c                   eval      t02ke1 = 'CONT'
109100100617     c                   call      'TIBS02R'
109200100617     c                   parm                    kpjba
109300100617     c                   parm                    tibs02ds
109400100617    2c                   if        t02err = *blanks
109500100617     c                   eval      dvpocont  = t02uni
109600100617    2c                   endif
109700030617     c                   if        o36pou <> 046
109800060915     c                   clear                   fnlv55ds
109900060915     c                   move      'L'           d55tla
110000060915     c                   move      o36pou        d55lin
110100100617     c                   if        wuda >= §vpoter
110200100617     c                   move      'E'           d55tpt
110300100617     c                   else
110400100617     c                   move      ' '           d55tpt
110500100617     c                   endif
110600060915     c                   z-add     wuda          d55drf
110700060915     c                   call      'FNLV55R'
110800060915     c                   parm                    fnlv55ds
110900060915     c                   z-add     d55tfp        termp             3 0
111000060915     c                   z-add     d55tfa        terma             3 0
111100060915      *se profilo di filiale e i terminal P/A sono uguali proteggo il campo
111200160908     c*m                 if        terma = termp
111300160908     c*m                 seton                                        20
111400160908     c*m                 endif
111500030617     c                   endif
111600000607      *
111700030710     C     Katt1         KLIST
111800030709     C                   KFLD                    vattter
111900030709     C                   KFLD                    vdata1g           8 0
112000030709     C                   KFLD                    vattvad
112100030709     C                   KFLD                    vattfgp
112200030709     C                   KFLD                    vattfga
112300030710     C                   KFLD                    vattprg
112400030710     C     Katt2         KLIST
112500030710     C                   KFLD                    vattter
112600030710     C                   KFLD                    vdata1g
112700030710     C                   KFLD                    vattvad
112800030710     C                   KFLD                    vattfgp
112900030710     C                   KFLD                    vattfga
113000030710     C                   KFLD                    vattprv
113100030709     C     Katt          KLIST
113200030709     C                   KFLD                    vattter
113300030711     C     Kattd         KLIST
113400030711     C                   KFLD                    vattter
113500030711     C                   KFLD                    vdata1g
113600030714     C     Kapd          KLIST
113700030714     C                   KFLD                    tipoa             1
113800030721     C                   KFLD                    vattpdrk          7 0
113900030714     C                   move      'D'           tipoa
114000030714
114100040209     c     kazcln        Klist
114200040209     c                   Kfld                    ktfp
114300040209     c                   Kfld                    ktfa
114400040209     c                   Kfld                    kann
114500040209     c                   Kfld                    kmes
114600050419      *
114700050419     c     ktbe          klist
114800050419     c                   kfld                    tbecod
114900050419     c                   kfld                    tbeke1
115000050419      *
115100051027     c     kfvv          Klist
115200060120     c                   Kfld                    wfgs
115300051027     c                   Kfld                    wnpg
115400051027     c                   Kfld                    wdfv
115500060120     c     kfvv2         Klist
115600060120     c                   Kfld                    wfgs
115700060120     c                   Kfld                    wnpg
115800051027      *
115900081119     c     kadd          Klist
116000081119     c                   Kfld                    vattvad
116100081119     c                   Kfld                    vattpdrk
116200081119     c                   Kfld                    vattfgp
116300081119     c                   Kfld                    vattfga
116400081119      *
116500090513     c     kaitra        klist
116600090513     c                   kfld                    adtpdr
116700090513     c                   kfld                    data0
116800090513
116900081119     c     kadt          Klist
117000081119     c                   Kfld                    addpdr
117100081119     c                   Kfld                    addprg
117200081119      *
117300090512     c     krco          klist
117400090512     c                   kfld                    socfor            3
117500090512     c                   kfld                    codfor            8
117600090512      *
117700090513     c     ktab          klist
117800090513     c                   kfld                    tblkut
117900090513     c                   kfld                    tblcod
118000090513     c                   kfld                    tblKEY
118100090513      *
118200050419      *reperisco limiti da tabella per calcolare il D-Day
118300050419     c                   movel(p)  '1'           tbeke1
118400050419     c                   movel     'BLD'         tbecod
118500050419     c     ktbe          chain     tntbe01l                           90
118600050419     c                   if        not *in90
118700050419     c                   movel     tbeuni        dbld
118800050419     c                   endif
118900090511     c* reperisce gg min durata allegato
119000090513     C                   clear                   ds5Aaut
119100090511     C                   movel     '5A'          tblcod
119200090511     C                   MOVEL(P)  'AUT'         tblKEY
119300090511     c                   z-add     1             tblkut
119400090511     C     KTAB          CHAIN     TABEL00F
119500090511     c                   if        %found(tabel00f)
119600090511     C                   movel     tbluni        ds5Aaut
119700090511     c                   end
119800040209
119900120628      * reperisco codice società traini
120000120628
120100120628     c                   move      '3I'          tblcod
120200120628     c                   movel(p)  '1'           tblkey
120300120628     c     ktab          chain     tabel00f
120400120628     c                   if        %found(tabel00f)
120500120628     c                   movel     tbluni        ds3i
120600120628     c                   eval      socieTRAZ = %subst(§3IBST: 7: 3)
120700120628     c                   endif
120800120628      *
120900120628     C*
121000011227     C                   ENDSR
121100081119      *---------------------------------------------------------------------
121200081119     C     repimporto    BEGSR
121300081119      *---------------------------------------------------------------------
121400081119     c                   clear                   importo          11 3
121500090717     c                   clear                   ADTCDFs           8
121600090717     c                   clear                   ADTCSFs           3
121700130508     C                   clear                   ficn5ads
121800130506      * reperisco importo del viaggio dal dettaglio e poi verifico la testata
121900130506     c     kadd          setll     fiadd02l
122000130506     c                   do        *hival
122100130506     c     kadd          reade     fiadd02l
122200130506     c                   if        %eof(fiadd02l)
122300130506     c                   leave
122400130506     c                   endif
122500130506     c                   if        addatb <> *blank
122600130506     c                   iter
122700130506     c                   endif
122800130506      * verifico se la testata è nel range di validità per il viaggio
122900130506     c     kadt          chain     fiadt01l
123000130506     c                   if        %found(fiadt01l) and
123100130506     c                             vdata1g >= adtddt and vdata1g <= adtdst
123200130506     c                             and adtdts > 0 and adtatb = *blank
123300130506     c                   z-add     addimp        importo
123400130506     c                   eval      ADTCDFs = ADTCDF
123500130506     c                   eval      ADTCSFs = ADTCSF
123600130506     c                   leave
123700130506     c                   endif
123800130506     c
123900130506     c                   enddo
124000130506      **
124100130506     c                   if        importo = 0
124200130506     c                   seton                                        9628
124300130506     c                   eval      $msg = 'Importo non reperito non trovata -
124400130506     c                             Tariffa valida per questo viaggio/autista'
124500130506     c                   leavesr
124600130506     c                   endif
124700130506      *    verifico sempre se si tratta di un trazionista e reperisco IVAFORNITORE
124800130506      *
124900130506      * ?Verifica se si tratta di trazionista o autista con contratto di città
125000130506     c                   clear                   tranrc
125100130506     c                   exsr      veriftraz
125200130506      * Se non è un trazionista verifico che la data e la tariffa siano congruenti con la
125300130506      * società operativa della filiale
125400130506      * ?  controlli autista non trazionista
125500130506     c                   if        esitotraz = ' '
125600130506     C*
125700130506     c
125800130506ab   c                   MOVE      vdata1g       dtVALIDITA
125900130506     c                   exsr      test_societa
126000130506ab   c                   if        adtcsf <> idsocieta
126100130506ab   c                   seton                                        2896
126200130506     c                   eval      $msg = 'Viaggio non Convalidabile -
126300130506     c                             con questa tariffa società operat-
126400130506     c                             iva incongruente'
126500130506     c                   leavesr
126600130506ab   C                   end
126700130508      *
126800130506      * verifiche su anagrafica autista
126900130506     c     kaitra        chain     aitra05l
127000130506      * autista accreditato
127100130506     c                   if        %found(aitra05l) and
127200130506     c                             tradin <= adtdst
127300130508     c                   movel     '§5AGGVALDR'  §5Atipochk
127400130506      *
127500130506     c                   else
127600130506      **
127700130506     c     adtpdr        setgt     aitra05l
127800130506     c     adtpdr        readpe    aitra05l
127900130506      * autista trovato ma disaccreditato
128000130506     c                   if        not %eof(aitra05l)
128100130506      **
128200130506     c                   if        tradfi < vdata1g
128300130506     c                                and tradfi > 0
128400130508     c                   clear                   §5Atipochk
128500130506     c                   endif
128600130506      **
128700130506     c                   else
128800130506      **
128900130508     c                   clear                   §5Atipochk
129000130506     c                   endif
129100130506     c                   endif
129200130506      **
129300130513     c                   if        tranrc <> 0
129400130513     c     tranrc        chain     aitrs01l
129500130513     c                   endif
129600130508      * ?__richiamo finestra valda e valdr per autisti di città controlli completi
129700130510     c                   if        *inkf
129800130508     c                   select
129900130513     c                   when      trsiva <> ivaforn and tranrc <> 0
130000130513     c                   movel     '§5AGGVALDA'  §5Atipochk
130100130513      * verifico se c'è corrispondenza fra partita iva fornitore e contratto
130200130508     c                   when      adtdrc > 0 and
130300130508     c                             §5Atipochk = '§5AGGVALDR'
130400130508     c                   leavesr
130500130508     c                   when      adtdrc > 0 and
130600130508     c                             §5Atipochk = ' '
130700130508     c                   movel     '§5AGGVALDA'  §5Atipochk
130800130508     c                   endsl
130900130508      *
131000130508     c                   z-add     vdata1g       §5adata1
131100130508     C                   z-add     adtdts        §5ADATA2
131200130508     c                   movel     adtpdr        §5Aautista
131300130508     c                   movel     adttsr        §5ARISTAMP
131400130508     c                   z-add     adtprg        §5APRGTAR
131500130508     c                   eval      §5AWINDOW  = 'F'
131600130508     C                   eval      savkpjbu    =  kpjbu
131700130508      * ?verifica parametri
131800130508     C                   MOVEL(p)  ficn5ads      kpjbu
131900130508     C                   call      'FICN5AR'
132000130508     C                   parm                    kpjba
132100130508     C                   eval      ficn5ads = kpjbu
132200130508     C                   clear                   kpjbu
132300130508     C                   MOVEL(p)  savkpjbu      kpjbu
132400130508     c                   if        §5aesito <> *blank
132500130508     c                   seton                                        9628
132600130508     c                   eval      $msg = 'Errore finestra controllo  '
132700130508     c                   leavesr
132800130508     c                   endif
132900130510     c                   endif
133000130508      * ?        ____________________
133100130506     c                   else
133200130506      **      x i contratti da TRAZIONISTI si controlla la data di fine Accreditamento
133300130506      * ?__controlli autista trazionista
133400130506     c                   IF        ADTnrc  >=9000000 and ADTnrc <=9999999
133500130508      **
133600130508     c                   if        adtdrc = 0
133700130508     c                   movel     '§5AGGVALDR'  §5Atipochk
133800130508     c                   else
133900130508     c                   leavesr
134000130508     c                   endif
134100130506      * richiamo pgm controllo per verificare se esistono valorizzazioni precedenti di xx giorni
134200130506      * effettuate con allegato non ancora rientrato firmato
134300130506      * ?__richiamo finestra valdr per trazionisti
134400130510     c                   if        *inkf
134500130508     c                   z-add     vdata1g       §5adata1
134600130508     C                   z-add     adtdts        §5ADATA2
134700130507     c                   movel     adtpdr        §5Aautista
134800130507     c                   movel     adttsr        §5ARISTAMP
134900130507     c                   z-add     adtprg        §5APRGTAR
135000130507     c                   eval      §5AWINDOW  = 'F'
135100130507     C                   eval      savkpjbu    =  kpjbu
135200130507     C                   MOVEL(p)  ficn5ads      kpjbu
135300130507     C                   call      'FICN5AR'
135400130507     C                   parm                    kpjba
135500130507     C                   eval      ficn5ads = kpjbu
135600130507     C                   clear                   kpjbu
135700130507     C                   MOVEL(p)  savkpjbu      kpjbu
135800130507     c                   if        §5aesito <> *blank
135900130507     c                   seton                                        9628
136000130507     c                   eval      $msg = 'Errore finestra controllo  '
136100130507     c                   leavesr
136200130510     c                   endif
136300130507     c                   endif
136400130506     c                   endif
136500120222      *
136600130507     c                   ENDIF
136700120222      ****
136800120222     C                   EndSR
136900090512      * ?___________________________________________________________________
137000090512     C     RepIvaFor     BegSR
137100090512      * ?___________________________________________________________________
137200090512     c                   clear                   ivaforn          16
137300090512     c* reperisco la p.iva del fornitore immesso in apd
137400090512     c                   move      adtcsf        socfor
137500090512     c                   move      adtcdf        codfor
137600090512     c     krco          chain     anfrn01l
137700090512     c                   if        %found(anfrn01l)
137800090512     c     frnsogg       chain     ansog01l
137900090512     c                   if        %found(ansog01l)
138000090512     c                   movel     sogpartiva    IvaForn
138100090512     c                   end
138200090512     c                   end
138300090512      *
138400090512     C                   ENDSR
138500120619     C**************************************************************************
138600120619     C*    CONTROLLo per a/d se il fornitore è un trazionista certificato
138700120619     C**************************************************************************
138800120619     C     veriftraz     BEGSR
138900120619      *
139000120619     c                   exsr      RepIvaFor
139100120619      *
139200120619     c                   clear                   esitotraz
139300120628     c                   clear                   partraz
139400120628     c                   clear                   dsAIATS
139500120619      *
139600120619     c                   if        IVAforn <> *blank
139700120619     c                   move      vdata1g       pardtintraz
139800120619     c                   move      vdata1g       pardtfitraz
139900120619     c                   movel     adtcsf        parsoctraz
140000120619     c                   movel     IVAforn       parivatraz
140100120619     c                   call      'TRULTRAZ'
140200120619     c                   parm                    partraz
140300120619     c                   parm                    dsAIATS
140400120619     c                   endif
140500120619     c**
140600120619     c                   endsr
140700030714      *---------------------------------------------------------------------
140800051027     C     comando       BEGSR
140900051027      *---------------------------------------------------------------------
141000051027     c                   if        %subst(knsif:7:1) = 'P'
141100051027     C                   movea     cmd1          cmd
141200051027     c                   else
141300051027     C                   movea     cmd2          cmd
141400051027     c                   endif
141500051027     C                   movel(p)  cmd           comman
141600051027     C                   call      'QCMDEXC'
141700051027     C                   parm                    comman           80
141800051027     C                   parm      48            lung             15 5
141900051027     C*
142000051027     C                   ENDSR
142100120628     C**************************************************************************
142200051027**         CMD1
142300060120OVRDBF FILE(FNFVV08L) TOFILE(FILTRAPRD/FNFVV08L)
142400051027**         CMD1
142500060120OVRDBF FILE(FNFVV08L) TOFILE(FILTRA201/FNFVV08L)
