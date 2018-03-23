000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200120807     h dftactgrp(*no) actgrp(*new)  BNDDIR('TIBS')
000300120807     H BNDDIR('TIBS') EXTBININT(*YES)
000400120705      *--------------------------------------------------------------*
000500030714     Fficn77d   CF   E             WORKSTN
000600030604     f                                     sfile(video2:snrr1)
000700030714     Ffiatt03l  uf   E           k disk
000800130522     Ffiatt13l  uf a E           k disk
000900081121     Ffiadt01l  if   E           k disk
001000081121     Ffiadd02l  if   E           k disk
001100030709     Fazorg01l  if   E           k disk
001200090630     Fansif01l  if   E           k disk
001300030714     Ffiapd01l  if   E           k disk
001400030725     Fanrco98j  if   E           k disk
001500090513     fAitrs01l  if   e           k Disk
001600090513     fAnfrn01l  if   e           k Disk
001700090513     fAitra05l  if   e           k Disk
001800090513     fAnsog01l  if   e           k Disk
001900090513     ftabel00f  if   e           k disk
002000090714     F*rtf100   O    F  100        PRINTER prtctl(prtds)
002100090714     fficn77p   o    e             printer
002200120705ab   ***************************************************************************
002300011228      *
002400020103     D Psds           SDS
002500020103     D  PgmName          *PROC
002600011228     D Kpjba         E DS
002700120306     D dsaiats       E DS                  EXTNAME(AIATS00F)
002800120502     d*
002900120306     D partraz         DS
003000120518     D  parsoc                        3s 0
003100120502     D  pariva                       16
003200120502     D  pardtin                       8s 0
003300120502     D  pardtfi                       8s 0
003400120502     d  esito                         1
003500120502     d  errmsg                       75
003600120705      *
003700131007     D uteautds      E DS
003800131007     d tibs42ds      e ds
003900130522     d ficn71ds      e ds
004000100617     d dvpocont      e ds
004100100617     d tibs02ds      e ds
004200030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004300030604     D CNCR80        E DS
004400030617     d Tibs36Ds      e ds
004500030722     D fnlv24ds      E DS
004600090513     D DS5Aaut       E DS
004700130509     d ficn5ads      e ds
004800011227     d data            ds            10
004900011228     d aa                      3      4
005000011228     d mm                      6      7
005100011228     d gg                      9     10
005200011228      *
005300030702     D fatto           S              1
005400030610     D modalita        S              1
005500030604     D x               S              4  0
005600011227     D WrkEofS01       S              1
005700011227     D WrkCarS01       S              1
005800011228     D $VIDEO          S             10
005900030604     D snrr1           S              5i 0
006000030722     D Savkpjbu        S                   like(kpjbu)
006100030604     D SavOpzione      S                   like(v1csce)
006200030709     D Savattter       S                   like(vattter)
006300030709     D Savattterd      S                   like(vattterd)
006400030606     d dataiso         s               d   datfmt(*iso)
006500030606     d dataeur         s               d   datfmt(*eur)
006600030725     D cost1           S             97
006700030725     D cost2           S             97
006800030725     D cost3           S             97
006900030725     D cost4           S             97
007000040614     D cost4b          S              7
007100040614     D cost5           S             57
007200030725     D cost6           S             97
007300030725     D cost7           S             30
007400090513     d datavaliso      s               d   datfmt(*iso)
007500090513     d dataconiso      s               d   datfmt(*iso)
007600090513     d data0           s              8  0
007700090703     D PARAM           DS
007800090703     d  parterm                       3  0
007900090703     d  pardat                        8  0
008000090703     d  parpdr                        7  0
008100090703     d  paresito                      1
008200030613     D WLBDA8          DS
008300030613     D  G02DAT                 1      8  0
008400030613     D  G02INV                 9     16  0
008500030613     D  G02ERR                17     17
008600030613     D  G02TGI                18     22  0
008700030725     d prtds           ds
008800030725     d   spab                         3  0
008900030725     d   spaa                         3  0
009000030725     d   skab                         3  0
009100030725     d   skaa                         3  0
009200030725     d   line                         3  0
009300090714     ***************************************************************************
009400090714     **
009500090714     ** Definizione costanti.
009600090714     **
009700090714     ***************************************************************************
009800090714     D tibsSocI0     e ds                  prefix(£) inz
009900090714     D tibsSocO0     e ds                  inz
010000090714     D prmRqsOpCode...
010100090714     D                 S             10A
010200090714     D prmRpyOpCode...
010300090714     D                 S             10A
010400090714     D prmRpyIdMsg...
010500090714     D                 S             10I 0
010600090714     D prmRqsFormato...
010700090714     D                 S             10A
010800090714     D prmRqsDataSize...
010900090714     D                 S             10I 0
011000090714     D prmRpyFormato...
011100090714     D                 S             10A
011200090714     D prmRpyDataSize...
011300090714     D                 S             10I 0
011400030701      *---------------- ----------------------------------------------------
011500120807ab   ** Prototipi.
011600120807ab    **
011700120807ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
011800120807ab    /DEFINE DFTACTGRP_NO
011900120807ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
012000120807ab    /UNDEFINE DFTACTGRP_NO
012100120807      *
012200120807ab   D TibsSof_esito...
012300120807ab   D                 S             10I 0 IMPORT
012400120807ab   D widSocieta...
012500120807EDPDCD                 S              3A
012600120807ab   **
012700120807EDPDCd WtVALIDITA      s               D
012800120807EDPDCd wALdatFINE      s               D
012900120807EDPDCd wALdatINIZ      s               D
013000120807      *---------------- Gestione formato video video01----------------------
013100120807ab   C*     Inizializza
013200120807ab    /FREE
013300120807ab     TibsSof_Init();
013400120807ab    /END-FREE
013500030604      *---------------- Gestione formato video video01----------------------
013600030604     C                   EXSR      Inzvideo1
013700030604     C                   DOU       $Video <> 'VIDEO1'
013800000608      *----------- Visualizzo dati di output in caso di errori.-------------
013900000608     C                   IF        *IN99
014000000608     C                   EVAL      *IN99 = *OFF
014100030604     C                   WRITE     video1
014200000608     C                   EVAL      *IN99 = *ON
014300000608     C                   ENDIF
014400000608      *
014500030702     c     emetti        tag
014600030604     C                   EXFMT     video1
014700000608      *
014800011227     C                   SELECT
014900030612     C                   WHEN      *INKC
015000011227     C                             OR
015100011227     C                             *INKL
015200011227     C                   EVAL      $Video = *BLANKS
015300011227
015400011227     C                   WHEN      *INKE
015500030604     C                   EXSR      Inzvideo1
015600011227     C                   WHEN      *INKB
015700030604     C                   EXSR      Chkvideo1
015800011227     C                   OTHER
015900030604     C                   EXSR      Chkvideo1
016000030715     c   43
016100030715     cor 44              goto      emetti
016200030604     C  N99              EXSR      Wrkvideo2c
016300031020     C     OK            CABEQ     0             EMETTI
016400011227     C                   ENDSL
016500011227      *
016600011227     C                   ENDDO
016700011227      *
016800011227     C                   EXSR      Uscita
016900011227     c**********************************************************************
017000011227     c* uscita
017100011227     c**********************************************************************
017200011227     C     Uscita        BEGSR
017300011227      *
017400011227     C                   EVAL      *INLR = *ON
017500011227     C                   RETURN
017600011227      *
017700011227     C                   ENDSR
017800011227     c**********************************************************************
017900011227     c* gestione sfl
018000011227     c**********************************************************************
018100030604     C     Wrkvideo2c    BEGSR
018200000627      *
018300011227     C                   EVAL      WrkCarS01 = *ON
018400030604     C                   EVAL      $Video = 'VIDEO2C'
018500011227      *
018600030604     C                   DOU       $Video <> 'VIDEO2C'
018700030604     C                   EXSR      Carvideo2
018800011228     c*Descrizione della ricerca
018900000627      *
019000031020     C     OK            CABEQ     0             TORNA
019100030604     C                   WRITE     video2z
019200030604     C                   EXFMT     video2c
019300011227     C                   SELECT
019400011228     c* fine
019500011227     C                   WHEN      *INKC=*ON
019600011227     C                   EVAL      $VIDEO=*BLANKS
019700011228     c* guida
019800011227     C                   WHEN      *INKL=*ON
019900030604     C                   EVAL      $VIDEO='VIDEO1'
020000011227
020100030714     C                   WHEN      *INKH=*ON
020200030714     C                   exsr      spalmaopz
020300030714
020400011227     C                   OTHER
020500011227
020600011227     C                   EXSR      GestioneSFL
020700011227     C                   ENDSL
020800011228
020900011227     C                   ENDDO
021000031020     C     torna         ENDSR
021100011227     c**********************************************************************
021200011227     c* Controlli video R01.
021300011227     c**********************************************************************
021400030604     C     Chkvideo1     BEGSR
021500000609      *
021600000608     C                   EVAL      *IN99 = *OFF
021700030715     C                   EVAL      *IN43 = *OFF
021800030908     C                   EVAL      *IN44 = *OFF
021900130812      * controllo autorizzazione del profilo
022000130812     c                   if        wuda    >= §vpoaut
022100131007     c                   clear                   tibs42ds
022200130812     c                   if        vattfgp = 001 or vattfgp = 101
022300130812     c                             or vattfgp = 102
022400131007     c                   movel     001           i42pge
022500130812     c                   else
022600131007     c                   movel     vattfgp       i42pge
022700130812     c                   endif
022800131007     c                   call      'TIBS42R'
022900131007     c                   parm                    tibs42ds
023000131007     c                   movel     o42uni        uteautds
023100130812     c                   if        §AFFCPSTD <> 'S'
023200130812     c                   seton                                        44
023300130812     c                   leavesr
023400130812     c                   endif
023500130812     c                   endif
023600120705      *
023700090630     c                   if        %subst(knmus:1:3) <> 'EDP'
023800030908     c                   if        o36pou = 001
023900030908     c                   select
024000030908     c                   when      vattfgp = 001 or vattfgp = 101
024100030908     c                             or vattfgp = 102
024200030908     c                   other
024300030908     c                   seton                                        44
024400030908     c                   endsl
024500030908     c                   endif
024600090630     c                   endif
024700030714     c     vattfgp       chain     azorg01l
024800090715     c                   if        %found(azorg01l)
024900030714     c                   movel     orgdes        vattfgpd
025000090722     C                   EVAL      orglocc = %TRIM(orgdes)+ ' ' +
025100090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
025200090722     C                             %TRIM(orgloc)
025300090715     c                   movel     orgloc        deslnp
025400090715     c                   else
025500090715     c                   clear                   vattfgpd
025600090715     c                   clear                   orglocc
025700090715     c                   clear                   deslnp
025800090715     c                   end
025900030709     c                   if        vdata1 > 0
026000030715     c                   move      vdata1        g02dat
026100030715     c                   move      *blank        g02err
026200030715     C                   CALL      'XSRDA8'
026300030715     C                   PARM                    WLBDA8
026400030715     C     G02ERR        IFEQ      '1'
026500030715     C                   SETON                                        43
026600030715     C                   END
026700030715     c                   move      g02dat        vdata1
026800030715     c                   move      g02inv        wuda
026900030715     c                   move      g02inv        vdata1g
027000030709     c                   endif
027100000608      *
027200000608     C                   ENDSR
027300011227     c**********************************************************************
027400011227     c* inizializza r01
027500011227     c**********************************************************************
027600030604     C     Inzvideo1     BEGSR
027700000608
027800030714     c                   move      o36pou        vattfgp
027900030709     c                   move      wdat          vdata1
028000030709     c                   move      wuda          vdata1g
028100030604     C                   EXSR      Chkvideo1
028200000609
028300000608     C                   ENDSR
028400030604     c**********************************************************************
028500030605     C     opzione       BEGSR
028600030605     c**********************************************************************
028700030610     c                   move      v1csce        modalita
028800130524     c                   setoff                                       2128
028900130524     c                   clear                   $msg
029000030605     c                   select
029100130522     c                   when      v1csce = '2' or
029200130522     c                             v1csce = 'P'
029300081121     c                   if        vattdcoh > 0
029400081121     c                   seton                                        2128
029500130524     c                   eval      $msg = 'Viaggi già confermato non -
029600081121     c                             modificabile'
029700130524     c                   leavesr
029800081121     c                   endif
029900030605     c                   exsr      modifica
030000030605     c                   when      v1csce = '5'
030100030606     c                   seton                                        21
030200030606     c                   exsr      modifica
030300030606     c                   setoff                                       21
030400030714     c                   when      v1csce = '6'
030500030725     c                   seton                                        21
030600030717     c                   exsr      modifica
030700030725     c  n28              exsr      stampa
030800030605     c                   endsl
030900030604     C                   ENDSR
031000030605     c**********************************************************************
031100030605     C     modifica      BEGSR
031200030605     c**********************************************************************
031300130524      * *in96 errore generico videata dettaglio
031400130524     c                   setoff                                       9628
031500130524     c                   setoff                                       464748
031600130524     c                   setoff                                       5241
031700130524     c                   clear                   vmsg
031800030714     c     katt1         chain     fiatt03l                           98
031900030610     c                   if        not *in98
032000030717     c                   if        attpmb = *blank and modalita ='6'or
032100030717     c                             attorpr = 0  and modalita ='6'
032200030717     c                   seton                                        2821
032300130524     c                   eval      vmsg = 'Non è possibile stampare il foglio -
032400030717     c                             se non sono stati inseriti i piombi e l''or-
032500030717     c                             ario reale di partenza'
032600030717     c                   endif
032700030709     c                   move      attVAD        vattVAD
032800030709     c                   move      attTER        vattTER
032900030709     c                   move      attFGP        vattFGP
033000030709     c                   move      attFGA        vattFGA
033100030709     c                   move      attDDc        vattDDc
033200090714     c                   move      attDDc        vattDDc1
033300090715     c                   move      attDDc        stadfv
033400030709     c                   move      attORP        vattORP
033500030714     c                   move      attORpr       vattORpr
033600030709     c                   move      attPRV        vattPRV
033700030728     c                   move      attPRg        vattPRg
033800030709     c                   z-add     attIMP        vattIMP
033900030709     c                   z-add     attIMPp       vattIMPp
034000130522     c                   movel(p)  attpmb        vattpmb
034100030709     c                   move      attPDR        vattPDR
034200030722     c                   move      attPDR        vattPDRk
034300081121     c                   move      attPDR        vattPDRh
034400030714     c     kapd          chain     fiapd01l                           95
034500090714     c  n95              movel     apdrsf        vapdrsf
034600090714     c  n95              movel     apdrsf        vattpdrd
034700030714     c   95              clear                   vattpdrd
034800090714     c   95              clear                   vapdrsf
034900030709     c                   move      attNOT        vattNOT
035000030714      * decodifica p.o.
035100030714     c     attter        chain     azorg01l                           95
035200030714     c  n95              movel     orgdes        vattterd
035300030714     c   95              clear                   vattterd
035400030701      * decodifica p.o.
035500030709     c     attfgp        chain     azorg01l                           95
035600030709     c  n95              movel     orgdes        vattfgpd
035700090722     C  n95              EVAL      orglocc = %TRIM(orgdes)+ ' ' +
035800090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
035900090722     C                             %TRIM(orgloc)
036000090715     c  n95              movel     orgloc        deslnp
036100030709     c   95              clear                   vattfgpd
036200090715     c   95              clear                   orglocc
036300090715     c                   clear                   deslnp
036400030709     c     attfga        chain     azorg01l                           95
036500030709     c  n95              movel     orgdes        vattfgad
036600090722     C  n95              EVAL      orglocs = %TRIM(orgdes)+ ' ' +
036700090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
036800090722     C                             %TRIM(orgloc)
036900030709     c   95              clear                   vattfgad
037000090715     c   95              clear                   orglocs
037100030701      *data decorrenza
037200030709     c                   if        attddc > 0
037300030709     c                   move      attddc        dataiso
037400030701     c                   move      dataiso       dataeur
037500030709     c                   move      dataeur       vattddc
037600090714     c                   move      dataeur       vattddc1
037700090715     c                   move      dataeur       stadfv
037800030701     c                   else
037900030709     c                   clear                   vattddc
038000090714     c                   clear                   vattddc1
038100090715     c                   clear                   stadfv
038200081121      *reperisce importo
038300081121     c                   if        attimp = 0 and attpdr > 0
038400081121     c                   exsr      repimporto
038500081121     c                   z-add     importo       vattimp
038600081121     c                   endif
038700030701     c                   endif
038800130522     c     katt1         chain     fiatt13l
038900130522     c                   if        %found(fiatt13l)
039000130522     c                   move      at1pmb        vattpmb
039100130522     c                   end
039200130522     c                   if        modalita = 'P' and not *in21
039300130522     c                   exsr      reppmb
039400130522     c                   movel     iO71Pmb       Vattpmb
039500130522     c                   end
039600030605     c                   exsr      Gesvideo3
039700030605     c                   endif
039800030605     C                   ENDSR
039900030714     c**********************************************************************
040000030714     C     stampa        BEGSR
040100030714     c**********************************************************************
040200030725      * anagrafica del fornitore
040300090714     c                   eval      stapkg = 12
040400090714     c                   clear                   sindpdr
040500030725     c     krco          chain     anrco98j
040600030725     c                   if        %Found(anrco98j)
040700090715     C                   EVAL      sindpdr = %TRIM(indindriz)+ ' ' +
040800090715     C                             %TRIM(indCAP) + ' ' +
040900090715     C                             %TRIM(indLocalit) + ' ' +
041000090715     C                             %TRIM(indprov) +
041100090715     C                             ' ' + 'Tel.' +
041200090715     C                             %TRIM(indtelefon) + ' ' +
041300090715     C                             'P.Iva' + ' ' +
041400090715     C                             %TRIM(sogpartiva)
041500030725     c                   endif
041600030725      * Società Bartolini con cui concordare la tariffa.
041700090714     ** Inizializzo il programma.
041800090714     C                   CALL      'TIBSSOCR'
041900090714     C                   PARM      'INIT'        prmRqsOpCode
042000090714     C                   PARM                    prmRpyOpCode
042100090714     C                   PARM                    prmRpyIdMsg
042200090714     ** Chiamo passando un id società Bartolini
042300090714     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
042400090714     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
042500090714     C                   RESET                   tibsSocI0
042600090722     C                   EVAL      £idSocieta = attsoc
042700110504     c                   move      attddc        dataiso
042800110504     C                   EVAL      £dtvalidita = dataiso
042900090714     C
043000090714     C                   CALL      'TIBSSOCR'
043100090714     C                   PARM      'GETANAGRAF'  prmRqsOpCode
043200090714     C                   PARM      *BLANK        prmRpyOpCode
043300090714     C                   PARM      *ZERO         prmRpyIdMsg
043400090714     C                   PARM      'TIBSSOCI0'   prmRqsFormato
043500090714     C                   PARM                    tibsSocI0
043600090714     C                   PARM                    prmRqsDataSize
043700090714     C                   PARM      'TIBSSOCO0'   prmRpyFormato
043800090714     C                   PARM                    tibsSocO0
043900090714     C                   PARM                    prmRpyDataSize
044000090714     ** Chiudo il programma.
044100090714     C                   CALL      'TIBSSOCR'
044200090714     C                   PARM      'FINALIZE'    prmRqsOpCode
044300090714     C                   PARM                    prmRpyOpCode
044400090714     C                   PARM                    prmRpyIdMsg
044500090715     c                   eval      ragsocs = RAGSOCIALE
044600090715     C                   EVAL      sedind = %TRIM(sedlegind) + ' ' +
044700090715     C                             %TRIM(sedlegCAP) + ' ' +
044800090715     C                             %TRIM(sedlegloc) + ' ' +
044900090715     C                             %TRIM(sedlegpro) +
045000090715     C                             ' ' + 'Tel.' + %TRIM(sedlegtel) +
045100090715     C                             ' ' + 'P.Iva' + ' ' +
045200090715     C                             %TRIM(partitaiva)
045300090714     c                   movel     ragsocbrev    ragsocbres
045400090714     c*    apdCSF        chain     ansif01l
045500090714     c*                  if        %Found(ansif01l)
045600090714     c*                  movel     sifRGS        rassoc           30
045700090714     c*                  endif
045800030725      *stampa del foglio
045900090714     c                   write     foglio
046000170621     c*                  write     firmaimg
046100090714     c                   write     foglio1
046200090714     c                   feod      ficn77p
046300090714     c*                  feod      prtf100
046400030714     c                   endsr
046500030605     c**********************************************************************
046600030605     C     gesvideo3     BEGSR
046700030605     c**********************************************************************
046800030702     c                   clear                   fatto
046900030605     c                   do        *hival
047000030605     c                   exfmt     video3
047100030605     c   kl              leave
047200130522     c* piombi
047300130522     c                   if        *inkm
047400130522     c                   exsr      reppmb
047500130522     c                   movel     iO71Pmb       Vattpmb
047600130522     c                   end
047700130522     c*
047800030624     c                   exsr      controv3
047900030717     c   28
048000030717     cor 96              iter
048100030605     c   kf              exsr      aggiorna
048200130522     c*
048300030702     c                   if        fatto <> *blank
048400030702     c                   leave
048500030702     c                   endif
048600030605
048700030605     c                   enddo
048800030605     C                   ENDSR
048900130522     c**********************************************************************
049000130522     C     reppmb        BEGSR
049100130522     c**********************************************************************
049200130522     c                   clear                   ficn71ds
049300130522     c                   eval      i71ter=vattter
049400130522     c                   eval      i71vad=vattvad
049500130522     c                   eval      i71fgp=vattfgp
049600130522     c                   eval      i71fga=vattfga
049700130522     c                   eval      i71prg=vattprg
049800130522     c                   move      vattddc       dataeur
049900130522     c                   move      dataeur       dataiso
050000130522     c                   move      dataiso       i71ddc
050100130522     c                   if        vattpdr <> ' '
050200130522     c                   movel     vattpdr       i71pdr
050300130522     c                   end
050400130522     c                   eval      I71ORP=vattorp
050500130522     c                   eval      io71pmb=vattpmb
050600130522     c                   movel     ficn71ds      kpjbu
050700130522     c                   call      'FICN71R'
050800130522     c                   parm                    kpjba
050900130522     c                   movel     kpjbu         ficn71ds
051000130522     C                   ENDSR
051100130522     c**********************************************************************
051200130522     C     srwrt10       BEGSR
051300130522     c**********************************************************************
051400130522     c                   clear                   fiatt100
051500130522     c                   move      attVAD        at1vad
051600130522     c                   move      attTER        at1ter
051700130522     c                   move      attFGP        at1fgp
051800130522     c                   move      attFGA        at1fga
051900130522     c                   move      attddc        at1ddc
052000130522     c                   move      attprg        at1prg
052100130522     c                   eval      at1pmb = %subst(vattPmb: 21: 30)
052200130522     c                   write     fiatt100
052300130522     c                   endsr
052400030605     c**********************************************************************
052500030624     C     Controv3      BEGSR
052600030605     c**********************************************************************
052700030613      *
052800030702      * *in96 errore generico videata dettaglio
052900090505     c                   setoff                                       9628
053000130523     c                   clear                   vmsg
053100030703      * decodifica p.o.
053200030709     c     vattfgp       chain     azorg01l                           95
053300030709     c  n95              movel     orgdes        vattfgpd
053400090722     C  n95              EVAL      orglocc = %TRIM(orgdes)+ ' ' +
053500090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
053600090722     C                             %TRIM(orgloc)
053700090715     c  n95              movel     orgloc        deslnp
053800030709     c   95              clear                   vattfgpd
053900090715     c   95              clear                   orglocc
054000090715     c   95              clear                   deslnp
054100030709     c     vattfga       chain     azorg01l                           95
054200030709     c  n95              movel     orgdes        vattfgad
054300090722     C  n95              EVAL      orglocs = %TRIM(orgdes)+ ' ' +
054400090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
054500090722     C                             %TRIM(orgloc)
054600030709     c   95              clear                   vattfgad
054700090715     c   95              clear                   orglocs
054800030701      *controlli data  decorrenza
054900030613     c                   setoff                                       50
055000030709     c                   if        vattddc > 0
055100030709     C                   MOVE      Vattddc       G02DAT
055200030613     C                   MOVEL     *BLANK        G02ERR
055300030613     C                   CALL      'XSRDA8'
055400030613     C                   PARM                    WLBDA8
055500030613     C     G02ERR        IFEQ      '1'
055600030701     C                   SETON                                        5096
055700030613     C                   END
055800030709     c                   move      g02dat        vattddc
055900090714     c                   move      g02dat        vattddc1
056000090715     c                   move      g02dat        stadfv
056100030709     c                   move      g02inv        vattddcg          8 0
056200030709     c                   endif
056300030714      * controllo autista
056400030722     c                   setoff                                       4847
056500030722     c                   if        vattpdr =  *zeros or vattpdr = *blank
056600130523     c                   eval      vmsg = 'CODICE AUT. OBBLIGATORIO !'
056700030722     c                   seton                                        4896
056800030722     c                   goto      endctr
056900030722     c                   endif
057000030722     C****  CODICE PADRONCINO  ****
057100030722     C     '?'           SCAN      vattpdr                                55
057200030722     C*
057300030722     C     *IN55         IFEQ      *ON
057400030722     C                   MOVEL     KPJBU         SAVKPJBU
057500030722     C                   Z-ADD     Vattter       d24FIL
057600030722     C                   Z-ADD     0             d24pdr
057700030722     C                   MOVEL     'R'           d24FLG
057800030722     C                   MOVEL     'D'           d24tip
057900030722     C                   MOVEL     *BLANKS       d24rsc
058000030722     C                   MOVEL(p)  fnlv24ds      KPJBU
058100030722     C                   CALL      'FNLV24R'
058200030722     C                   PARM                    KPJBA
058300030722     C                   MOVEL     KPJBU         fnlv24ds
058400030722     C                   MOVEL     savKPJBU      KPJBU
058500030722     C*
058600030722     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
058700030722     C     d24pdr        IFNE      0
058800030722     C                   MOVEL     *ZEROS        Vattpdr
058900030722     C                   MOVEL     d24pdr        Vattpdr
059000030722     C                   MOVEL     vattpdr       Vattpdrk
059100030722     C                   MOVEL     d24rsc        vattpdrd
059200031027     c                   else
059300031027     C                   clear                   Vattpdr
059400031027     C                   clear                   Vattpdrk
059500031027     C                   clear                   vattpdrd
059600130523     c                   eval      vmsg = 'CODICE AUT. OBBLIGATORIO !'
059700031027     c                   seton                                        4896
059800031027     c                   goto      endctr
059900030722     C                   ENDIF
060000030722     C*
060100030722     C                   ENDIF
060200030722     c                   movel     vattpdr       como3             3 0
060300030722     C                   MOVEL     vattpdr       Vattpdrk
060400030722     c                   if        como3 <> vattter
060500130523     c                   eval      vmsg = 'Codice autista errato,asseg-
060600130523     c                             nato ad altro terminal o inesistente'
060700030722     c                   seton                                        4796
060800030722     c                   goto      endctr
060900030722     c                   else
061000030722     c     kapd          chain     fiapd01l                           95
061100080206     c                   if        not *in95 and apdksc <> 0
061200080207     c                             and apdatb = *blank
061300090507     c                   movel     apdrsf        vattpdrd
061400080206     c                   else
061500080206     c                   clear                   vattpdrd
061600130523     c                   eval      vmsg = 'Codice autista errato,asseg-
061700130523     c                             nato ad altro terminal o inesistente'
061800080206     c                   seton                                        4796
061900080206     c                   endif
062000030722     c                   endif
062100081121      *reperisce importo
062200081121     c                   exsr      repimporto
062300081121     c                   z-add     importo       vattimp
062400081121      *Importo obbligatorio
062500090630     c                   if        vattimpp =  *zeros or
062600090630     c                             vattpdrk <> vattpdrh
062700081121     c                   z-add     vattimp       vattimpp
062800090630     c                   z-add     vattpdrk      vattpdrh
062900081121     c                   endif
063000030714      *controllo piombi
063100030714     c                   setoff                                       46
063200030714     c                   if        vattpmb  = *blank
063300130523     c                   eval      vmsg = 'Piombi obbligatori'
063400030714     c                   seton                                        4696
063500030722     c                   goto      endctr
063600030714     c                   endif
063700030702      *controllo ore
063800030702     c                   setoff                                       52
063900030714     c                   if        vattorpr = *zeros
064000130523     c                   eval      vmsg = 'Ora partenza reale obbligatoria'
064100030702     c                   seton                                        5296
064200030722     c                   goto      endctr
064300030702     c                   endif
064400090508      *se importi diversi obbligo inserimento nota
064500090508     c                   setoff                                       41
064600090508     c                   if        vattimpp <> vattimp and vattnot = *blank
064700130523     c                   eval      vmsg = 'Inserimento nota obbligator-
064800130523     c                             io se i prezzi differenti'
064900090508     c                   seton                                        4196
065000090508     c                   endif
065100030722     C     endctr        ENDSR
065200030605     c**********************************************************************
065300030605     C     aggiorna      BEGSR
065400030605     c**********************************************************************
065500030714     c                   move      vattORPr      attORPr
065600030709     c                   z-add     vattIMPp      attIMPp
065700030722     c                   if        vattpdr <> *zeros and vattpdr <> *blank
065800030722     c                   move      vattPDR       attPDR
065900030722     c                   else
066000030722     c                   clear                   attPDR
066100030722     c                   endif
066200030709     c                   move      vattNOT       attNOT
066300130522     c                   movel     vattpmb       attpmb
066400090716     c                   eval      attcdf = ADTCDFs
066500090716     c                   eval      attsoc = ADTCSFs
066600030714     c                   movel     vattpmb       vattpmb2
066700030717     c                   if        modalita = '6'
066800030717     c                   z-add     wudate        attdst
066900030717     c                   z-add     wdat          vattdst
067000030717     c                   endif
067100130522     c* piombi
067200130522     c     katt1         chain     fiatt13l
067300130522     c                   if        %found(fiatt13l)
067400130522     c                   if        %subst(vattpmb: 21: 30) <> ' '
067500130522     c                   eval      at1pmb = %subst(vattpmb: 21: 30)
067600130522     c                   update    fiatt100
067700130522     c                   else
067800130522     c                   delete    fiatt100
067900130522     c                   end
068000130522     c                   else
068100130522     c                   if        %subst(vattpmb: 21: 30) <> ' '
068200130522     c                   exsr      srwrt10
068300130522     c                   endif
068400130522     c                   endif
068500030717
068600030709     c                   update    fiatt000
068700030702     c                   move      'X'           fatto
068800030605     C                   ENDSR
068900030714     c**********************************************************************
069000030714     C     spalmaopz     BEGSR
069100090505     c**********************************************************************
069200030714     c                   do        *hival        recor             4 0
069300030714     c     recor         chain     video2                             93
069400030714     c   93              leave
069500030714     c                   move      '6'           v1csce
069600030714     c                   update    video2
069700030714     c                   enddo
069800030714     C                   ENDSR
069900011228     c**********************************************************************
070000011228     c* carica sfl
070100011228     c**********************************************************************
070200030604     C     Carvideo2     BEGSR
070300011228     C                   IF        WrkCarS01 = *ON
070400030604     C                   EVAL      snrr1   = 0
070500011228     C                   EVAL      *IN90 = *ON
070600020219     C                   EVAL      *IN91 = *OFF
070700030604     C                   WRITE     video2c
070800011228     C                   EVAL      *IN90 = *OFF
070900030714     c     kattd         setll     fiatt03l
071000030701     c                   do        *hival
071100030714     c     kattd         reade     fiatt03l                               97
071200030701     c   97              leave
071300030701     c                   move      *blanks       v1csce
071400030709     c                   move      attVAD        vattVAD
071500030709     c                   move      attTER        vattTER
071600081121     c                   z-add     attdco        vattdcoh
071700030709     c     attter        chain     azorg01l                           95
071800030709     c  n95              movel     orgdes        vattterd
071900030709     c   95              clear                   vattterd
072000030709     c                   move      attFGP        vattFGP
072100030709     c     attfgp        chain     azorg01l                           95
072200030714     c  n95              movel     orgdes        vattfgpd
072300090722     C  n95              EVAL      orglocc = %TRIM(orgdes)+ ' ' +
072400090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
072500090722     C                             %TRIM(orgloc)
072600090715     c  n95              movel     orgloc        deslnp
072700030714     c   95              clear                   vattfgpd
072800090715     c   95              clear                   orglocc
072900090715     c   95              clear                   deslnp
073000030709     c                   move      attFGA        vattFGA
073100030709     c     attfga        chain     azorg01l                           95
073200030709     c  n95              movel     orgdes        vattfgad2
073300030709     c   95              clear                   vattfgad2
073400030714     c                   move      attORPr       vattORPr
073500030709     c                   move      attPRV        vattPRV
073600030710     c                   move      attPRg        vattPRg
073700030722     c                   if        attpdr > 0
073800030722     c                   move      attPDR        vattPDR
073900030722     c                   else
074000030722     c                   clear                   vattPDR
074100030722     c                   endif
074200030722     c                   move      vattpdr       vattpdrk
074300030722     c     kapd          chain     fiapd01l                           95
074400090507     c  n95              movel     apdrsf        vattpdrd2
074500030722     c   95              clear                   vattpdrd2
074600030717     c                   movel     attpmb        vattpmb2
074700030717     c                   if        attdst > 0
074800030717     c                   move      attdst        dataiso
074900030717     c                   move      dataiso       dataeur
075000030717     c                   move      dataeur       vattdst
075100030717     c                   else
075200030717     c                   clear                   vattdst
075300030717     c                   endif
075400030717
075500030701      *esco per raggiunta massima capacità sfl
075600030617     c                   if        snrr1 > 9990
075700030617     c                   leave
075800030617     c                   endif
075900030617
076000030604     C                   EVAL      snrr1 = snrr1 + 1
076100030604     C                   WRITE     video2
076200011228     C                   ENDDO
076300011228
076400030604     C                   IF        snrr1  > 0
076500020219     C                   EVAL      *IN91 = *ON
076600020219     C                   MOVE      1             OK                1 0
076700020219     C                   ELSE
076800020219     C                   MOVE      0             OK                1 0
076900030701     C                   ENDIF
077000030612     C                   EVAL      nrr1  = 1
077100011228     C                   ENDIF
077200020219     C                   EVAL      WrkCarS01 = *OFF
077300030604     C                   EVAL      snrr1  = 1
077400030604     C                   EVAL      csrrrn = 1
077500011228     C                   ENDSR
077600011227     c**********************************************************************
077700011227     c* gestione sfl
077800011227     c**********************************************************************
077900000613     C     GestioneSFL   BEGSR
078000000627      *
078100000627     C                   EVAL      WrkEofS01 = *OFF
078200030605      * Elaborazione opzioni.
078300011228     c*
078400030605     c                   do        *hival        X                 4 0
078500030605     c     X             CHAIN     video2                             50
078600030605     C   50              LEAVE
078700030604     C                   IF        v1csce <> *blanks
078800030605     c                   exsr      opzione
078900030611
079000030611     c                   move      *blank        v1csce
079100030605     c                   update    video2
079200030611
079300011228     C                   ENDIF
079400030605     C                   enddo
079500000627      *
079600030703     C                   ENDSR
079700130509      *---------------------------------------------------------------------
079800130509     C     repimporto    BEGSR
079900130509      *---------------------------------------------------------------------
080000130509     c                   clear                   importo          11 3
080100130509     c                   clear                   ADTCDFs           8
080200130509     c                   clear                   ADTCSFs           3
080300130509     C                   clear                   ficn5ads
080400130509      * reperisco importo del viaggio dal dettaglio e poi verifico la testata
080500130509     c     kadd          setll     fiadd02l
080600130509     c                   do        *hival
080700130509     c     kadd          reade     fiadd02l
080800130509     c                   if        %eof(fiadd02l)
080900130509     c                   leave
081000130509     c                   endif
081100130509     c                   if        addatb <> *blank
081200130509     c                   iter
081300130509     c                   endif
081400130509      * verifico se la testata è nel range di validità per il viaggio
081500130509     c     kadt          chain     fiadt01l
081600130509     c                   if        %found(fiadt01l) and
081700130509     c                             vdata1g >= adtddt and vdata1g <= adtdst
081800130509     c                             and adtdts > 0 and adtatb = *blank
081900130509     c                   z-add     addimp        importo
082000130509     c                   eval      ADTCDFs = ADTCDF
082100130509     c                   eval      ADTCSFs = ADTCSF
082200130509     c                   leave
082300130509     c                   endif
082400130509     c
082500130509     c                   enddo
082600130509      **
082700130509     c                   if        importo = 0
082800130509     c                   seton                                        9628
082900130523     c                   eval      vmsg = 'Importo non reperito non trovata -
083000130509     c                             Tariffa valida per questo viaggio/autista'
083100130509     c                   leavesr
083200130509     c                   endif
083300130509      *    verifico sempre se si tratta di un trazionista e reperisco IVAFORNITORE
083400130509      *
083500130509      * ?Verifica se si tratta di trazionista o autista con contratto di città
083600130509     c                   clear                   tranrc
083700130509     c                   clear                   prtnia
083800130509     c                   clear                   prtpia
083900130509      *
084000130509      *******
084100130509     c                   clear                   aitrs000
084200130509     c                   clear                   aitra000
084300130509     c                   clear                   dsaiats
084400130509     c                   exsr      veriftraz
084500130509      * Se non è un trazionista verifico che la data e la tariffa siano congruenti con la
084600130509      * società operativa della filiale
084700130509      * ?  controlli autista non trazionista
084800130509     c                   if        esito = ' '
084900130509     C*
085000170621     c*m                 eval      COMMITEN =
085100170621     c*m                           'ALBERTO VIGNOCCHI Nato a Bologna il 24/06/-
085200170621     c*m                           1964'
085300130509     c                   setoff                                       88
085400130509     c
085500130509ab   c                   MOVE      vdata1g       wtVALIDITA
085600130509     c                   exsr      test_societa
085700130509ab   c                   if        adtcsf <> widsocieta
085800130509ab   c                   seton                                        2896
085900130523     c                   eval      vmsg = 'Viaggio non Convalidabile -
086000130509     c                             con questa tariffa società operat-
086100130509     c                             iva incongruente'
086200130509     c                   leavesr
086300130509ab   C                   end
086400130509      *
086500130509      * verifico se c'è corrispondenza fra partita iva fornitore e contratto
086600130509      * verifiche su anagrafica autista
086700130509     c     kaitra        chain     aitra05l
086800130509      * autista accreditato
086900130509     c                   if        %found(aitra05l) and
087000130509     c                             tradin <= adtdst
087100130509     c                   movel     '§5AGGVALDR'  §5Atipochk
087200130509      *
087300130509     c                   else
087400130509      **
087500130509     c     adtpdr        setgt     aitra05l
087600130509     c     adtpdr        readpe    aitra05l
087700130509      * autista trovato ma disaccreditato
087800130509     c                   if        not %eof(aitra05l)
087900130509      **
088000130509     c                   if        tradfi < vdata1g
088100130509     c                                and tradfi > 0
088200130509     c                   clear                   §5Atipochk
088300130509     c                   endif
088400130509      **
088500130509     c                   else
088600130509      **
088700130509      * autista non trovato è da accreditare
088800130509      * verifica se è nel limite dei giorni consentiti di attesa x l'accreditamento
088900130509     c                   clear                   §5Atipochk
089000130509     c                   endif
089100130509     c                   endif
089200130510     c                   if        tranrc <> 0
089300130510     c     tranrc        chain     aitrs01l
089400130510      **
089500130510     c                   eval        prtnia  =   trsnia
089600130510     c                   eval        prtpia  =   trspia
089700130513     c                   endif
089800130509      **
089900130509      * ?__richiamo finestra valda e valdr per autisti di città controlli completi
090000130510     c                   if        *inkf
090100130509     c                   select
090200130513     c                   when      trsiva <> ivaforn and tranrc <> 0
090300130513     c                   movel     '§5AGGVALDA'  §5Atipochk
090400130513     c                   when      adtdrc > 0 and
090500130509     c                             §5Atipochk = '§5AGGVALDR'
090600140729     c                   leavesr
090700130509     c                   when      adtdrc > 0 and
090800130509     c                             §5Atipochk = ' '
090900130509     c                   movel     '§5AGGVALDA'  §5Atipochk
091000130509     c                   endsl
091100130509      *
091200130509     c                   z-add     vdata1g       §5adata1
091300130509     C                   z-add     adtdts        §5ADATA2
091400130509     c                   movel     adtpdr        §5Aautista
091500130509     c                   movel     adttsr        §5ARISTAMP
091600130509     c                   z-add     adtprg        §5APRGTAR
091700130509     c                   eval      §5AWINDOW  = 'F'
091800130509     C                   eval      savkpjbu    =  kpjbu
091900130509      * ?verifica parametri
092000130509     C                   MOVEL(p)  ficn5ads      kpjbu
092100130509     C                   call      'FICN5AR'
092200130509     C                   parm                    kpjba
092300130509     C                   eval      ficn5ads = kpjbu
092400130509     C                   clear                   kpjbu
092500130509     C                   MOVEL(p)  savkpjbu      kpjbu
092600130509     c                   if        §5aesito <> *blank
092700130509     c                   seton                                        9628
092800130523     c                   eval      vmsg = 'Errore finestra controllo  '
092900130509     c                   leavesr
093000130509     c                   endif
093100130510     c                   endif
093200130509      * ?        ____________________
093300130509     c                   else
093400130509      * ?controlli autista trazionista
093500170621     c*m                 eval      COMMITEN =
093600170621     c*m                           'DAMIANO PEDINI Nato a Pavia il 24/12/-
093700170621     c*m                           1968'
093800130509     c                   seton                                        88
093900130509      * iscr.albo p.iva
094000130509     c                   eval        prtnia  =   atsnia
094100130509     c                   eval        prtpia  =   atspia
094200130509      **      x i contratti da TRAZIONISTI si controlla la data di fine Accreditamento
094300130509     c                   IF        ADTnrc  >=9000000 and ADTnrc <=9999999
094400130509     c                   if        adtdrc = 0
094500130509     c                   movel     '§5AGGVALDR'  §5Atipochk
094600130509     c                   else
094700130509     c                   leavesr
094800130509     c                   endif
094900130509      * richiamo pgm controllo per verificare se esistono valorizzazioni precedenti di xx giorni
095000130509      * effettuate con allegato non ancora rientrato firmato
095100130509      * ?__richiamo finestra valdr per trazionisti
095200130510     c                   if        *inkf
095300130509     c                   z-add     vdata1g       §5adata1
095400130509     C                   z-add     adtdts        §5ADATA2
095500130509     c                   movel     adtpdr        §5Aautista
095600130509     c                   movel     adttsr        §5ARISTAMP
095700130509     c                   z-add     adtprg        §5APRGTAR
095800130509     c                   eval      §5AWINDOW  = 'F'
095900130509     C                   eval      savkpjbu    =  kpjbu
096000130509     C                   MOVEL(p)  ficn5ads      kpjbu
096100130509     C                   call      'FICN5AR'
096200130509     C                   parm                    kpjba
096300130509     C                   eval      ficn5ads = kpjbu
096400130509     C                   clear                   kpjbu
096500130509     C                   MOVEL(p)  savkpjbu      kpjbu
096600130509     c                   if        §5aesito <> *blank
096700130509     c                   seton                                        9628
096800130523     c                   eval      vmsg = 'Errore finestra controllo  '
096900130509     c                   leavesr
097000130509     c                   endif
097100130510     c                   endif
097200130509     c                   endif
097300130509      *
097400130509     c                   ENDIF
097500130509      ****
097600130509     C                   EndSR
097700090513      * ?___________________________________________________________________
097800090513     C     RepIvaFor     BegSR
097900090513      * ?___________________________________________________________________
098000090513     c                   clear                   ivaforn          16
098100090513     c* reperisco la p.iva del fornitore immesso in apd
098200090513     c                   move      adtcsf        socfor
098300090513     c                   move      adtcdf        codfor
098400090513     c     krco2         chain     anfrn01l
098500090513     c                   if        %found(anfrn01l)
098600090513     c     frnsogg       chain     ansog01l
098700090513     c                   if        %found(ansog01l)
098800090513     c                   movel     sogpartiva    IvaForn
098900090513     c                   end
099000090513     c                   end
099100090513      *
099200090513     C                   ENDSR
099300120807ab   C**************************************************************************
099400120807ab   C     TEST_SOCieta  begSR
099500120807ab    *
099600120807ab    /FREE
099700120807ab
099800120807EDPDC  widSocieta = TibsSof_GetIdSocietaByIdFiliale( vattter
099900120807EDPDC                                             : wtVALIDITA
100000120807EDPDC                                             : 'O'
100100120807EDPDC                                             : walDatIniz
100200120807EDPDC                                             : walDatFine
100300120807ab                                                );
100400120807ab
100500120807EDPDC  IF widSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
100600120807EDPDC    // Gestire l'errore.
100700120807ab     ENDIF;
100800120807ab
100900120807ab    /END-FREE
101000120807     c                   move      waldatfine    valdatfineW       8 0
101100120807ab   C                   ENDSR
101200011227     c**********************************************************************
101300000607     C     *INZSR        BEGSR
101400011227     c**********************************************************************
101500000607      *
101600000607     C     *ENTRY        PLIST
101700000607     C                   PARM                    Kpjba
101800030604     C                   Z-ADD     1             CODUT
101900030604     C                   CALL      'X§PARUT'
102000030604     C                   PARM                    UTEDSE
102100030604     C                   MOVEL     REC80         CNCR80
102200030703     C                   MOVEL     Ragut         rsut
102300030703     C                   MOVEL     knsif         vknsif
102400030703     C                   MOVEL     knmus         vknmus
102500030702      *reperimento data
102600030702     C                   TIME                    W0120            14 0
102700030702     C                   MOVE      W0120         WDAT              8 0
102800030702     C*
102900030702     C                   Z-ADD     WDAT          G02DAT
103000030702     C                   MOVEL     *BLANK        G02ERR
103100030702     C                   CALL      'XSRDA8'
103200030702     C                   PARM                    WLBDA8
103300030702     C* UDATE A 8 IN AAAA/MM/GG
103400030717     C                   Z-ADD     G02INV        WUDAte            8 0
103500030702     C                   Z-ADD     G02INV        WUDA              8 0
103600030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
103700030617     C                   CLEAR                   Tibs36Ds
103800030617     C                   EVAL      I36ute = knmus
103900030617     C                   CALL      'TIBS36R'
104000030617     C                   PARM                    tibs36ds
104100100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
104200100617     c                   clear                   tibs02ds
104300100617     c                   clear                   dvpocont
104400100617     c                   eval      t02mod = 'C'
104500100617     c                   eval      t02sif = knsif
104600100617     c                   eval      t02cod = 'VPO'
104700100617     c                   eval      t02ke1 = 'CONT'
104800100617     c                   call      'TIBS02R'
104900100617     c                   parm                    kpjba
105000100617     c                   parm                    tibs02ds
105100100617    2c                   if        t02err = *blanks
105200100617     c                   eval      dvpocont  = t02uni
105300100617    2c                   endif
105400100318
105500030714     c                   movel     i36ute        edp3              3
105600030908     c                   if        edp3 <> 'EDP' and o36pou <> 001
105700030714     c                   seton                                        20
105800030714     c                   endif
105900000607      *
106000030711     C     Kattd         KLIST
106100030714     C                   KFLD                    vattfgp
106200030714     C                   KFLD                    vdata1g           8 0
106300030714     C     Katt1         KLIST
106400030714     C                   KFLD                    vattfgp
106500030714     C                   KFLD                    vdata1g
106600030714     C                   KFLD                    vattprg
106700030714     C     Kapd          KLIST
106800030714     C                   KFLD                    tipoa             1
106900030722     C                   KFLD                    vattpdrk          7 0
107000030714     C                   move      'D'           tipoa
107100030725     c     krco          klist
107200090717     c                   kfld                    attsoc
107300030725     c                   kfld                    knatura           1
107400090717     c                   kfld                    attcdf
107500030725     c                   movel     'F'           knatura
107600030725
107700081121      *
107800081121     c     kadd          Klist
107900081121     c                   Kfld                    vattvad
108000081121     c                   Kfld                    vattpdrk
108100081121     c                   Kfld                    vattfgp
108200081121     c                   Kfld                    vattfga
108300081121      *
108400081121     c     kadt          Klist
108500081121     c                   Kfld                    addpdr
108600081121     c                   Kfld                    addprg
108700081121      *
108800090513     c     kaitra        klist
108900090513     c                   kfld                    adtpdr
109000090513     c                   kfld                    data0
109100090513
109200090513     c     krco2         klist
109300090513     c                   kfld                    socfor            3
109400090513     c                   kfld                    codfor            8
109500090513      *
109600090513     c     ktab          klist
109700090513     c                   kfld                    tblkut
109800090513     c                   kfld                    tblcod
109900090513     c                   kfld                    tblKEY
110000090513      *
110100030725     c                   eval      cost1 ='Io sottoscritto mi impegno ad azion-
110200030725     c                             are l''antifurto ogni volta che abbandono -
110300030725     c                             per qualsiasi motivo '
110400030725     c                   eval      cost2 ='l''automezzo e mi impegno a rendere -
110500030725     c                             immediatamente operativo l''antifurto blocca-
110600030725     c                             porte.'
110700030725     c                   eval      cost3 ='Prendo atto che sono stati applicati-
110800030725     c                              i piombi sotto annotati'
110900030725     c
111000030725     c                   eval      cost4 ='Dichiaro altresi di essere stato in-
111100040614     c                             formato ai sensi dell''art.13 D.Lgs. 196/20-
111200040614     c                             03 ed autorizzo la'
111300040614     c                   eval      cost4b='società'
111400040614     c                   eval      cost5 ='ai sensi degli art. 23 e 26 del D. -
111500040614     c                             Lgs. citato, al'
111600040614     c                   eval      cost6 = 'trattamento-
111700040614     c                              e alla comunicazione a soggetti terzi-
111800040614     c                              dei miei dati.'
111900030725     c                   move      *all'_'       cost7
112000090513     c* reperisce gg min durata allegato
112100090513     C                   clear                   ds5Aaut
112200090513     C                   movel     '5A'          tblcod
112300090513     C                   MOVEL(P)  'AUT'         tblKEY
112400090513     c                   z-add     1             tblkut
112500090513     C     KTAB          CHAIN     TABEL00F
112600090513     c                   if        %found(tabel00f)
112700090513     C                   movel     tbluni        ds5Aaut
112800090513     c                   end
112900090513
113000011227     C                   ENDSR
113100120306     C**************************************************************************
113200120306     C*    CONTROLLo per a/d se il fornitore è un trazionista certificato
113300120306     C**************************************************************************
113400120306     C     veriftraz     BEGSR
113500120306      *
113600120306      *   con la Partita IVa
113700120306     c                   exsr      RepIvaFor
113800120306      *
113900120306     c                   clear                   esito
114000120306     c                   if        IVAforn <> *blank
114100120306     c                   clear                   partraz
114200120612     c                   move      vdata1g       pardtin
114300120612     c                   move      vdata1g       pardtfi
114400120518     c                   movel     adtcsf        parsoc
114500120306     c                   movel     IVAforn       pariva
114600120306     c                   call      'TRULTRAZ'
114700120306     c                   parm                    partraz
114800120306     c                   parm                    dsAIATS
114900120306     c                   endif
115000120306     c**
115100120306     c                   endsr
