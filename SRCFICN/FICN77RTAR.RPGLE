000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300030714     Fficn77d   CF   E             WORKSTN
000400030604     f                                     sfile(video2:snrr1)
000500030714     Ffiatt03l  uf   E           k disk
000600081121     Ffiadt01l  if   E           k disk
000700081121     Ffiadd02l  if   E           k disk
000800030709     Fazorg01l  if   E           k disk
000900090630     Fansif01l  if   E           k disk
001000030714     Ffiapd01l  if   E           k disk
001100030725     Fanrco98j  if   E           k disk
001200090513     fAitrs01l  if   e           k Disk
001300090513     fAnfrn01l  if   e           k Disk
001400090513     fAitra05l  if   e           k Disk
001500090513     fAnsog01l  if   e           k Disk
001600090513     ftabel00f  if   e           k disk
001700090714     F*rtf100   O    F  100        PRINTER prtctl(prtds)
001701090714     fficn77p   o    e             printer
001800011228      *
001900020103     D Psds           SDS
002000020103     D  PgmName          *PROC
002100011228     D Kpjba         E DS
002200011228      *
002201100318     D daut          E DS
002202100318     d ficn70ds      e ds
002203100617     d dvpocont      e ds
002204100617     d tibs02ds      e ds
002300030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002400030604     D CNCR80        E DS
002500030617     d Tibs36Ds      e ds
002600030722     D fnlv24ds      E DS
002700090513     D DS5Aaut       E DS
002800011227     d data            ds            10
002900011228     d aa                      3      4
003000011228     d mm                      6      7
003100011228     d gg                      9     10
003200011228      *
003300030702     D fatto           S              1
003400030610     D modalita        S              1
003500030604     D x               S              4  0
003600011227     D WrkEofS01       S              1
003700011227     D WrkCarS01       S              1
003800011228     D $VIDEO          S             10
003900030604     D snrr1           S              5i 0
004000030722     D Savkpjbu        S                   like(kpjbu)
004100030604     D SavOpzione      S                   like(v1csce)
004200030709     D Savattter       S                   like(vattter)
004300030709     D Savattterd      S                   like(vattterd)
004400030606     d dataiso         s               d   datfmt(*iso)
004500030606     d dataeur         s               d   datfmt(*eur)
004600030725     D cost1           S             97
004700030725     D cost2           S             97
004800030725     D cost3           S             97
004900030725     D cost4           S             97
005000040614     D cost4b          S              7
005100040614     D cost5           S             57
005200030725     D cost6           S             97
005300030725     D cost7           S             30
005400090513     d datavaliso      s               d   datfmt(*iso)
005500090513     d dataconiso      s               d   datfmt(*iso)
005600090513     d data0           s              8  0
005700090513     d limaccr         s              4  0
005800090513     d limcopia        s              4  0
005900090703     D PARAM           DS
006000090703     d  parterm                       3  0
006100090703     d  pardat                        8  0
006200090703     d  parpdr                        7  0
006300090703     d  paresito                      1
006400030613     D WLBDA8          DS
006500030613     D  G02DAT                 1      8  0
006600030613     D  G02INV                 9     16  0
006700030613     D  G02ERR                17     17
006800030613     D  G02TGI                18     22  0
006900030725     d prtds           ds
007000030725     d   spab                         3  0
007100030725     d   spaa                         3  0
007200030725     d   skab                         3  0
007300030725     d   skaa                         3  0
007400030725     d   line                         3  0
007402090714     ***************************************************************************
007403090714     **
007404090714     ** Definizione costanti.
007405090714     **
007406090714     ***************************************************************************
007410090714     D tibsSocI0     e ds                  prefix(£) inz
007413090714     D tibsSocO0     e ds                  inz
007416090714     D prmRqsOpCode...
007417090714     D                 S             10A
007418090714     D prmRpyOpCode...
007419090714     D                 S             10A
007420090714     D prmRpyIdMsg...
007421090714     D                 S             10I 0
007422090714     D prmRqsFormato...
007423090714     D                 S             10A
007424090714     D prmRqsDataSize...
007425090714     D                 S             10I 0
007426090714     D prmRpyFormato...
007427090714     D                 S             10A
007428090714     D prmRpyDataSize...
007429090714     D                 S             10I 0
007500030701      *---------------- ----------------------------------------------------
007600030604      *---------------- Gestione formato video video01----------------------
007700030604     C                   EXSR      Inzvideo1
007800030604     C                   DOU       $Video <> 'VIDEO1'
007900000608      *----------- Visualizzo dati di output in caso di errori.-------------
008000000608     C                   IF        *IN99
008100000608     C                   EVAL      *IN99 = *OFF
008200030604     C                   WRITE     video1
008300000608     C                   EVAL      *IN99 = *ON
008400000608     C                   ENDIF
008500000608      *
008600030702     c     emetti        tag
008700030604     C                   EXFMT     video1
008800000608      *
008900011227     C                   SELECT
009000030612     C                   WHEN      *INKC
009100011227     C                             OR
009200011227     C                             *INKL
009300011227     C                   EVAL      $Video = *BLANKS
009400011227
009500011227     C                   WHEN      *INKE
009600030604     C                   EXSR      Inzvideo1
009700011227     C                   WHEN      *INKB
009800030604     C                   EXSR      Chkvideo1
009900011227     C                   OTHER
010000030604     C                   EXSR      Chkvideo1
010100030715     c   43
010200030715     cor 44              goto      emetti
010300030604     C  N99              EXSR      Wrkvideo2c
010400031020     C     OK            CABEQ     0             EMETTI
010500011227     C                   ENDSL
010600011227      *
010700011227     C                   ENDDO
010800011227      *
010900011227     C                   EXSR      Uscita
011000011227     c**********************************************************************
011100011227     c* uscita
011200011227     c**********************************************************************
011300011227     C     Uscita        BEGSR
011400011227      *
011500011227     C                   EVAL      *INLR = *ON
011600011227     C                   RETURN
011700011227      *
011800011227     C                   ENDSR
011900011227     c**********************************************************************
012000011227     c* gestione sfl
012100011227     c**********************************************************************
012200030604     C     Wrkvideo2c    BEGSR
012300000627      *
012400011227     C                   EVAL      WrkCarS01 = *ON
012500030604     C                   EVAL      $Video = 'VIDEO2C'
012600011227      *
012700030604     C                   DOU       $Video <> 'VIDEO2C'
012800030604     C                   EXSR      Carvideo2
012900011228     c*Descrizione della ricerca
013000000627      *
013100031020     C     OK            CABEQ     0             TORNA
013200030604     C                   WRITE     video2z
013300030604     C                   EXFMT     video2c
013400011227     C                   SELECT
013500011228     c* fine
013600011227     C                   WHEN      *INKC=*ON
013700011227     C                   EVAL      $VIDEO=*BLANKS
013800011228     c* guida
013900011227     C                   WHEN      *INKL=*ON
014000030604     C                   EVAL      $VIDEO='VIDEO1'
014100011227
014200030714     C                   WHEN      *INKH=*ON
014300030714     C                   exsr      spalmaopz
014400030714
014500011227     C                   OTHER
014600011227
014700011227     C                   EXSR      GestioneSFL
014800011227     C                   ENDSL
014900011228
015000011227     C                   ENDDO
015100031020     C     torna         ENDSR
015200011227     c**********************************************************************
015300011227     c* Controlli video R01.
015400011227     c**********************************************************************
015500030604     C     Chkvideo1     BEGSR
015600000609      *
015700000608     C                   EVAL      *IN99 = *OFF
015800030715     C                   EVAL      *IN43 = *OFF
015900030908     C                   EVAL      *IN44 = *OFF
016000090630     c                   if        %subst(knmus:1:3) <> 'EDP'
016100030908     c                   if        o36pou = 001
016200030908     c                   select
016300030908     c                   when      vattfgp = 001 or vattfgp = 101
016400030908     c                             or vattfgp = 102
016500030908     c                   other
016600030908     c                   seton                                        44
016700030908     c                   endsl
016800030908     c                   endif
016900090630     c                   endif
017000030714     c     vattfgp       chain     azorg01l
017001090715     c                   if        %found(azorg01l)
017100030714     c                   movel     orgdes        vattfgpd
017101090722     C                   EVAL      orglocc = %TRIM(orgdes)+ ' ' +
017102090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
017103090722     C                             %TRIM(orgloc)
017105090715     c                   movel     orgloc        deslnp
017106090715     c                   else
017107090715     c                   clear                   vattfgpd
017108090715     c                   clear                   orglocc
017109090715     c                   clear                   deslnp
017110090715     c                   end
017200030709     c                   if        vdata1 > 0
017300030715     c                   move      vdata1        g02dat
017400030715     c                   move      *blank        g02err
017500030715     C                   CALL      'XSRDA8'
017600030715     C                   PARM                    WLBDA8
017700030715     C     G02ERR        IFEQ      '1'
017800030715     C                   SETON                                        43
017900030715     C                   END
018000030715     c                   move      g02dat        vdata1
018100030715     c                   move      g02inv        wuda
018200030715     c                   move      g02inv        vdata1g
018300030709     c                   endif
018400000608      *
018500000608     C                   ENDSR
018600011227     c**********************************************************************
018700011227     c* inizializza r01
018800011227     c**********************************************************************
018900030604     C     Inzvideo1     BEGSR
019000000608
019100030714     c                   move      o36pou        vattfgp
019200030709     c                   move      wdat          vdata1
019300030709     c                   move      wuda          vdata1g
019400030604     C                   EXSR      Chkvideo1
019500000609
019600000608     C                   ENDSR
019700030604     c**********************************************************************
019800030605     C     opzione       BEGSR
019900030605     c**********************************************************************
020000030610     c                   move      v1csce        modalita
020100030605     c                   select
020200030605     c                   when      v1csce = '2'
020300081121     c                   if        vattdcoh > 0
020400081121     c                   seton                                        2128
020500081121     c                   eval      $msg = 'Viaggi già confermato non -
020600081121     c                             modificabile'
020700081121     c                   endif
020800030605     c                   exsr      modifica
020900081121     c                   setoff                                       2128
021000081121     c                   clear                   $msg
021100030605     c                   when      v1csce = '5'
021200030606     c                   seton                                        21
021300030606     c                   exsr      modifica
021400030606     c                   setoff                                       21
021500030714     c                   when      v1csce = '6'
021600030725     c                   seton                                        21
021700030717     c                   exsr      modifica
021800030725     c  n28              exsr      stampa
021900030725     c   28              clear                   $msg
022000030717     c                   setoff                                       2128
022100030605     c                   endsl
022200030604     C                   ENDSR
022300030605     c**********************************************************************
022400030605     C     modifica      BEGSR
022500030605     c**********************************************************************
022600030714     c     katt1         chain     fiatt03l                           98
022700030610     c                   if        not *in98
022800030717     c                   if        attpmb = *blank and modalita ='6'or
022900030717     c                             attorpr = 0  and modalita ='6'
023000030717     c                   seton                                        2821
023100030717     c                   eval      $msg = 'Non è possibile stampare il foglio -
023200030717     c                             se non sono stati inseriti i piombi e l''or-
023300030717     c                             ario reale di partenza'
023400030717     c                   endif
023500030709     c                   move      attVAD        vattVAD
023600030709     c                   move      attTER        vattTER
023700030709     c                   move      attFGP        vattFGP
023800030709     c                   move      attFGA        vattFGA
023900030709     c                   move      attDDc        vattDDc
023901090714     c                   move      attDDc        vattDDc1
023902090715     c                   move      attDDc        stadfv
024000030709     c                   move      attORP        vattORP
024100030714     c                   move      attORpr       vattORpr
024200030709     c                   move      attPRV        vattPRV
024300030728     c                   move      attPRg        vattPRg
024400030709     c                   z-add     attIMP        vattIMP
024500030709     c                   z-add     attIMPp       vattIMPp
024600030714     c                   movel     attpmb        vattpmb
024601100927     c                   movel     attflr        vattflr
024700030709     c                   move      attPDR        vattPDR
024800030722     c                   move      attPDR        vattPDRk
024900081121     c                   move      attPDR        vattPDRh
025000030714     c     kapd          chain     fiapd01l                           95
025100090714     c  n95              movel     apdrsf        vapdrsf
025101090714     c  n95              movel     apdrsf        vattpdrd
025200030714     c   95              clear                   vattpdrd
025201090714     c   95              clear                   vapdrsf
025300030709     c                   move      attNOT        vattNOT
025400030714      * decodifica p.o.
025500030714     c     attter        chain     azorg01l                           95
025600030714     c  n95              movel     orgdes        vattterd
025700030714     c   95              clear                   vattterd
025800030701      * decodifica p.o.
025900030709     c     attfgp        chain     azorg01l                           95
026000030709     c  n95              movel     orgdes        vattfgpd
026003090722     C  n95              EVAL      orglocc = %TRIM(orgdes)+ ' ' +
026004090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
026006090722     C                             %TRIM(orgloc)
026007090715     c  n95              movel     orgloc        deslnp
026100030709     c   95              clear                   vattfgpd
026101090715     c   95              clear                   orglocc
026103090715     c                   clear                   deslnp
026200030709     c     attfga        chain     azorg01l                           95
026300030709     c  n95              movel     orgdes        vattfgad
026302090722     C  n95              EVAL      orglocs = %TRIM(orgdes)+ ' ' +
026303090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
026305090722     C                             %TRIM(orgloc)
026400030709     c   95              clear                   vattfgad
026401090715     c   95              clear                   orglocs
026500030701      *data decorrenza
026600030709     c                   if        attddc > 0
026700030709     c                   move      attddc        dataiso
026800030701     c                   move      dataiso       dataeur
026900030709     c                   move      dataeur       vattddc
026901090714     c                   move      dataeur       vattddc1
026902090715     c                   move      dataeur       stadfv
027000030701     c                   else
027100030709     c                   clear                   vattddc
027101090714     c                   clear                   vattddc1
027102090715     c                   clear                   stadfv
027200081121      *reperisce importo
027300081121     c                   if        attimp = 0 and attpdr > 0
027400081121     c                   exsr      repimporto
027500081121     c                   z-add     importo       vattimp
027600081121     c                   endif
027700030701     c                   endif
027800030605     c                   exsr      Gesvideo3
027900030605     c                   endif
028000030605     C                   ENDSR
028100030714     c**********************************************************************
028200030714     C     stampa        BEGSR
028300030714     c**********************************************************************
028400030725      * anagrafica del fornitore
028601090714     c                   eval      stapkg = 12
028603090714     c                   clear                   sindpdr
028700030725     c     krco          chain     anrco98j
028800030725     c                   if        %Found(anrco98j)
028801090715     C                   EVAL      sindpdr = %TRIM(indindriz)+ ' ' +
028802090715     C                             %TRIM(indCAP) + ' ' +
028803090715     C                             %TRIM(indLocalit) + ' ' +
028804090715     C                             %TRIM(indprov) +
028805090715     C                             ' ' + 'Tel.' +
028806090715     C                             %TRIM(indtelefon) + ' ' +
028807090715     C                             'P.Iva' + ' ' +
028808090715     C                             %TRIM(sogpartiva)
029000030725     c                   endif
029100030725      * Società Bartolini con cui concordare la tariffa.
029201090714     ** Inizializzo il programma.
029202090714     C                   CALL      'TIBSSOCR'
029203090714     C                   PARM      'INIT'        prmRqsOpCode
029204090714     C                   PARM                    prmRpyOpCode
029205090714     C                   PARM                    prmRpyIdMsg
029206090714     ** Chiamo passando un id società Bartolini
029207090714     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
029208090714     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
029209090714     C                   RESET                   tibsSocI0
029210090722     C                   EVAL      £idSocieta = attsoc
029211110504     c                   move      attddc        dataiso
029212110504     C                   EVAL      £dtvalidita = dataiso
029213090714     C
029214090714     C                   CALL      'TIBSSOCR'
029215090714     C                   PARM      'GETANAGRAF'  prmRqsOpCode
029216090714     C                   PARM      *BLANK        prmRpyOpCode
029217090714     C                   PARM      *ZERO         prmRpyIdMsg
029218090714     C                   PARM      'TIBSSOCI0'   prmRqsFormato
029219090714     C                   PARM                    tibsSocI0
029220090714     C                   PARM                    prmRqsDataSize
029221090714     C                   PARM      'TIBSSOCO0'   prmRpyFormato
029222090714     C                   PARM                    tibsSocO0
029223090714     C                   PARM                    prmRpyDataSize
029224090714     ** Chiudo il programma.
029225090714     C                   CALL      'TIBSSOCR'
029226090714     C                   PARM      'FINALIZE'    prmRqsOpCode
029227090714     C                   PARM                    prmRpyOpCode
029228090714     C                   PARM                    prmRpyIdMsg
029229090715     c                   eval      ragsocs = RAGSOCIALE
029230090715     C                   EVAL      sedind = %TRIM(sedlegind) + ' ' +
029231090715     C                             %TRIM(sedlegCAP) + ' ' +
029232090715     C                             %TRIM(sedlegloc) + ' ' +
029233090715     C                             %TRIM(sedlegpro) +
029234090715     C                             ' ' + 'Tel.' + %TRIM(sedlegtel) +
029235090715     C                             ' ' + 'P.Iva' + ' ' +
029236090715     C                             %TRIM(partitaiva)
029237090714     c                   movel     ragsocbrev    ragsocbres
029300090714     c*    apdCSF        chain     ansif01l
029400090714     c*                  if        %Found(ansif01l)
029500090714     c*                  movel     sifRGS        rassoc           30
029600090714     c*                  endif
029700030725      *stampa del foglio
029800090714     c                   write     foglio
029801090714     c                   write     firmaimg
029802090714     c                   write     foglio1
029900090714     c                   feod      ficn77p
029901090714     c*                  feod      prtf100
030000030714     c                   endsr
030100030605     c**********************************************************************
030200030605     C     gesvideo3     BEGSR
030300030605     c**********************************************************************
030400030702     c                   clear                   fatto
030500030605     c                   do        *hival
030600030605     c                   exfmt     video3
030700030605     c   kl              leave
030800030624     c                   exsr      controv3
030900030717     c   28
031000030717     cor 96              iter
031100030605     c   kf              exsr      aggiorna
031200030702     c                   if        fatto <> *blank
031300030702     c                   leave
031400030702     c                   endif
031500030605
031600030605     c                   enddo
031700030605     C                   ENDSR
031800030605     c**********************************************************************
031900030624     C     Controv3      BEGSR
032000030605     c**********************************************************************
032100030613      *
032200030702      * *in96 errore generico videata dettaglio
032300090505     c                   setoff                                       9628
032400090505     c                   clear                   $msg
032500030703      * decodifica p.o.
032600030709     c     vattfgp       chain     azorg01l                           95
032700030709     c  n95              movel     orgdes        vattfgpd
032703090722     C  n95              EVAL      orglocc = %TRIM(orgdes)+ ' ' +
032704090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
032706090722     C                             %TRIM(orgloc)
032707090715     c  n95              movel     orgloc        deslnp
032800030709     c   95              clear                   vattfgpd
032802090715     c   95              clear                   orglocc
032803090715     c   95              clear                   deslnp
032900030709     c     vattfga       chain     azorg01l                           95
033000030709     c  n95              movel     orgdes        vattfgad
033002090722     C  n95              EVAL      orglocs = %TRIM(orgdes)+ ' ' +
033003090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
033005090722     C                             %TRIM(orgloc)
033100030709     c   95              clear                   vattfgad
033101090715     c   95              clear                   orglocs
033200030701      *controlli data  decorrenza
033300030613     c                   setoff                                       50
033400030709     c                   if        vattddc > 0
033500030709     C                   MOVE      Vattddc       G02DAT
033600030613     C                   MOVEL     *BLANK        G02ERR
033700030613     C                   CALL      'XSRDA8'
033800030613     C                   PARM                    WLBDA8
033900030613     C     G02ERR        IFEQ      '1'
034000030701     C                   SETON                                        5096
034100030613     C                   END
034200030709     c                   move      g02dat        vattddc
034201090714     c                   move      g02dat        vattddc1
034202090715     c                   move      g02dat        stadfv
034300030709     c                   move      g02inv        vattddcg          8 0
034400030709     c                   endif
034500030714      * controllo autista
034600030722     c                   setoff                                       4847
034700030722     c                   if        vattpdr =  *zeros or vattpdr = *blank
034800030722     c                   seton                                        4896
034900030722     c                   goto      endctr
035000030722     c                   endif
035100030722     C****  CODICE PADRONCINO  ****
035200030722     C     '?'           SCAN      vattpdr                                55
035300030722     C*
035400030722     C     *IN55         IFEQ      *ON
035500030722     C                   MOVEL     KPJBU         SAVKPJBU
035600030722     C                   Z-ADD     Vattter       d24FIL
035700030722     C                   Z-ADD     0             d24pdr
035800030722     C                   MOVEL     'R'           d24FLG
035900030722     C                   MOVEL     'D'           d24tip
036000030722     C                   MOVEL     *BLANKS       d24rsc
036100030722     C                   MOVEL(p)  fnlv24ds      KPJBU
036200030722     C                   CALL      'FNLV24R'
036300030722     C                   PARM                    KPJBA
036400030722     C                   MOVEL     KPJBU         fnlv24ds
036500030722     C                   MOVEL     savKPJBU      KPJBU
036600030722     C*
036700030722     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
036800030722     C     d24pdr        IFNE      0
036900030722     C                   MOVEL     *ZEROS        Vattpdr
037000030722     C                   MOVEL     d24pdr        Vattpdr
037100030722     C                   MOVEL     vattpdr       Vattpdrk
037200030722     C                   MOVEL     d24rsc        vattpdrd
037300031027     c                   else
037400031027     C                   clear                   Vattpdr
037500031027     C                   clear                   Vattpdrk
037600031027     C                   clear                   vattpdrd
037700031027     c                   seton                                        4896
037800031027     c                   goto      endctr
037900030722     C                   ENDIF
038000030722     C*
038100030722     C                   ENDIF
038200030722     c                   movel     vattpdr       como3             3 0
038300030722     C                   MOVEL     vattpdr       Vattpdrk
038400030722     c                   if        como3 <> vattter
038500030722     c                   seton                                        4796
038600030722     c                   goto      endctr
038700030722     c                   else
038800030722     c     kapd          chain     fiapd01l                           95
038900080206     c                   if        not *in95 and apdksc <> 0
039000080207     c                             and apdatb = *blank
039100090507     c                   movel     apdrsf        vattpdrd
039200080206     c                   else
039300080206     c                   clear                   vattpdrd
039400080206     c                   seton                                        4796
039500080206     c                   endif
039600030722     c                   endif
039700081121      *reperisce importo
039800090630     c*                  if        vattimp = 0 and vattpdrk > 0 or
039900090630     c*                            vattpdrk <> vattpdrh
040000081121     c                   exsr      repimporto
040100081121     c                   z-add     importo       vattimp
040200090630     c*                  z-add     importo       vattimpp
040300090630     c*                  endif
040400081121      *Importo obbligatorio
040500090630     c                   if        vattimpp =  *zeros or
040600090630     c                             vattpdrk <> vattpdrh
040700081121     c                   z-add     vattimp       vattimpp
040800090630     c                   z-add     vattpdrk      vattpdrh
040900081121     c                   endif
041000030714      *controllo piombi
041100030714     c                   setoff                                       46
041200030714     c                   if        vattpmb  = *blank
041300030714     c                   seton                                        4696
041400030722     c                   goto      endctr
041500030714     c                   endif
041600030702      *controllo ore
041700030702     c                   setoff                                       52
041800030714     c                   if        vattorpr = *zeros
041900030702     c                   seton                                        5296
042000030722     c                   goto      endctr
042100030702     c                   endif
042200090508      *se importi diversi obbligo inserimento nota
042300090508     c                   setoff                                       41
042400090508     c                   if        vattimpp <> vattimp and vattnot = *blank
042500090508     c                   seton                                        4196
042600090508     c                   endif
042700030722     C     endctr        ENDSR
042800030605     c**********************************************************************
042900030605     C     aggiorna      BEGSR
043000030605     c**********************************************************************
043100030714     c                   move      vattORPr      attORPr
043200030709     c                   z-add     vattIMPp      attIMPp
043300030722     c                   if        vattpdr <> *zeros and vattpdr <> *blank
043400030722     c                   move      vattPDR       attPDR
043500030722     c                   else
043600030722     c                   clear                   attPDR
043700030722     c                   endif
043800030709     c                   move      vattNOT       attNOT
043900030710     c                   move      vattpmb       attpmb
043901100927     c                   move      vattflr       attflr
044100090716     c                   eval      attcdf = ADTCDFs
044200090716     c                   eval      attsoc = ADTCSFs
044300030714     c                   movel     vattpmb       vattpmb2
044400030717     c                   if        modalita = '6'
044500030717     c                   z-add     wudate        attdst
044600030717     c                   z-add     wdat          vattdst
044700030717     c                   endif
044800030717
044900030709     c                   update    fiatt000
045000030702     c                   move      'X'           fatto
045100030605     C                   ENDSR
045200030714     c**********************************************************************
045300030714     C     spalmaopz     BEGSR
045400090505     c**********************************************************************
045500030714     c                   do        *hival        recor             4 0
045600030714     c     recor         chain     video2                             93
045700030714     c   93              leave
045800030714     c                   move      '6'           v1csce
045900030714     c                   update    video2
046000030714     c                   enddo
046100030714     C                   ENDSR
046200011228     c**********************************************************************
046300011228     c* carica sfl
046400011228     c**********************************************************************
046500030604     C     Carvideo2     BEGSR
046600011228     C                   IF        WrkCarS01 = *ON
046700030604     C                   EVAL      snrr1   = 0
046800011228     C                   EVAL      *IN90 = *ON
046900020219     C                   EVAL      *IN91 = *OFF
047000030604     C                   WRITE     video2c
047100011228     C                   EVAL      *IN90 = *OFF
047200030714     c     kattd         setll     fiatt03l
047300030701     c                   do        *hival
047400030714     c     kattd         reade     fiatt03l                               97
047500030701     c   97              leave
047600030701     c                   move      *blanks       v1csce
047700030709     c                   move      attVAD        vattVAD
047800030709     c                   move      attTER        vattTER
047900081121     c                   z-add     attdco        vattdcoh
048000030709     c     attter        chain     azorg01l                           95
048100030709     c  n95              movel     orgdes        vattterd
048200030709     c   95              clear                   vattterd
048300030709     c                   move      attFGP        vattFGP
048400030709     c     attfgp        chain     azorg01l                           95
048500030714     c  n95              movel     orgdes        vattfgpd
048502090722     C  n95              EVAL      orglocc = %TRIM(orgdes)+ ' ' +
048503090722     C                             '-' + ' ' + %TRIM(orgind) + ' ' +
048505090722     C                             %TRIM(orgloc)
048506090715     c  n95              movel     orgloc        deslnp
048600030714     c   95              clear                   vattfgpd
048601090715     c   95              clear                   orglocc
048603090715     c   95              clear                   deslnp
048700030709     c                   move      attFGA        vattFGA
048800030709     c     attfga        chain     azorg01l                           95
048900030709     c  n95              movel     orgdes        vattfgad2
049000030709     c   95              clear                   vattfgad2
049100030714     c                   move      attORPr       vattORPr
049200030709     c                   move      attPRV        vattPRV
049300030710     c                   move      attPRg        vattPRg
049400030722     c                   if        attpdr > 0
049500030722     c                   move      attPDR        vattPDR
049600030722     c                   else
049700030722     c                   clear                   vattPDR
049800030722     c                   endif
049900030722     c                   move      vattpdr       vattpdrk
050000030722     c     kapd          chain     fiapd01l                           95
050100090507     c  n95              movel     apdrsf        vattpdrd2
050200030722     c   95              clear                   vattpdrd2
050300030717     c                   movel     attpmb        vattpmb2
050400030717     c                   if        attdst > 0
050500030717     c                   move      attdst        dataiso
050600030717     c                   move      dataiso       dataeur
050700030717     c                   move      dataeur       vattdst
050800030717     c                   else
050900030717     c                   clear                   vattdst
051000030717     c                   endif
051100030717
051200030701      *esco per raggiunta massima capacità sfl
051300030617     c                   if        snrr1 > 9990
051400030617     c                   leave
051500030617     c                   endif
051600030617
051700030604     C                   EVAL      snrr1 = snrr1 + 1
051800030604     C                   WRITE     video2
051900011228     C                   ENDDO
052000011228
052100030604     C                   IF        snrr1  > 0
052200020219     C                   EVAL      *IN91 = *ON
052300020219     C                   MOVE      1             OK                1 0
052400020219     C                   ELSE
052500020219     C                   MOVE      0             OK                1 0
052600030701     C                   ENDIF
052700030612     C                   EVAL      nrr1  = 1
052800011228     C                   ENDIF
052900020219     C                   EVAL      WrkCarS01 = *OFF
053000030604     C                   EVAL      snrr1  = 1
053100030604     C                   EVAL      csrrrn = 1
053200011228     C                   ENDSR
053300011227     c**********************************************************************
053400011227     c* gestione sfl
053500011227     c**********************************************************************
053600000613     C     GestioneSFL   BEGSR
053700000627      *
053800000627     C                   EVAL      WrkEofS01 = *OFF
053900030605      * Elaborazione opzioni.
054000011228     c*
054100030605     c                   do        *hival        X                 4 0
054200030605     c     X             CHAIN     video2                             50
054300030605     C   50              LEAVE
054400030604     C                   IF        v1csce <> *blanks
054500030605     c                   exsr      opzione
054600030611
054700030611     c                   move      *blank        v1csce
054800030605     c                   update    video2
054900030611
055000011228     C                   ENDIF
055100030605     C                   enddo
055200000627      *
055300030703     C                   ENDSR
055400081121      *---------------------------------------------------------------------
055500081121     C     repimporto    BEGSR
055600081121      *---------------------------------------------------------------------
055700081121     c                   clear                   importo          11 3
055701090716     c                   clear                   ADTCDFs           8
055702090716     c                   clear                   ADTCSFs           3
055800090703      * richiamo pgm controllo per verificare se esistono valorizzazioni precedenti di xx giorni
055900090703      * effettuate con allegato non ancora rientrato firmato
056000090703     c                   move      vdata1g       dataiso
056100090703     c     dataiso       subdur    §5AGGVALDR:*d dataiso
056200090703     c                   move      dataiso       pardat
056300090703     c                   move      vattpdrk      parpdr
056400090703     c                   movel     vattpdrk      parterm
056500090703     c                   clear                   paresito
056600090703     c                   call      'FICN76R1'
056700090703     C                   parm                    param
056800090703     c                   if        paresito <> *blank
056900090703     c                   seton                                        9628
057000090703     c                   eval      $msg = 'Esistono valorizzazioni precedenti -
057100090703     c                             con allegato non rientrato VERIFICARE '
057200090703     c                   leavesr
057300090703     c                   endif
057400081121      * reperisco importo del viaggio dal dettaglio e poi verifico la testata
057500081121     c     kadd          setll     fiadd02l
057600081121     c                   do        *hival
057700081121     c     kadd          reade     fiadd02l
057800081121     c                   if        %eof(fiadd02l)
057900081121     c                   leave
058000081121     c                   endif
058100090703     c                   if        addatb <> *blank
058200090703     c                   iter
058300090703     c                   endif
058400081121      * verifico se la testata è nel range di validità per il viaggio
058500081121     c     kadt          chain     fiadt01l
058600081121     c                   if        %found(fiadt01l) and
058700081121     c                             vdata1g >= adtddt and vdata1g <= adtdst
058800090703     c                             and adtdts > 0 and adtatb = *blank
058900081121     c                   z-add     addimp        importo
058901090716     c                   eval      ADTCDFs = ADTCDF
058902090716     c                   eval      ADTCSFs = ADTCSF
059000081121     c                   leave
059100081121     c                   endif
059200081121     c
059300081121     c                   enddo
059400090505     c                   if        importo = 0
059500090505     c                   seton                                        9628
059600090505     c                   eval      $msg = 'Importo non reperito non trovata -
059700090505     c                             Tariffa valida per questo viaggio/autista'
059800090513     c                   leavesr
059900090505     c                   endif
060000090513      * imposto valore giorni per calcolo attesa accreditamento e rientro copia firmata
060100090513      * se periodo speciale(trazioni ridotte) somma giorni in più per l'attesa
060200090513     c                   if        vdata1g >= §5ADTAINI and
060300090513     c                             vdata1g <= §5ADTAFIN
060400090513     c                   eval      limaccr = §5AGGVALDA + §5AGGINPIU
060500090513     c                   eval      limcopia = §5AGGVALDR + §5AGGINPIU
060600090513     c                   else
060700090513     c                   eval      limaccr = §5AGGVALDA
060800090513     c                   eval      limcopia = §5AGGVALDR
060900090513     c                   endif
061000090629      * se ristampa aggiungo in qualsiasi caso ulteriori giorni
061100090629     c                   if        adttsr = 'R'
061200090629     c                   eval      limaccr = limaccr + §5aggris
061300090629     c                   eval      limcopia = limcopia + §5aggris
061400090629     c                   endif
061401090715     c*
061402090715     c                   clear                   tranrc
061500090513     c* verifiche per limiti date
061600090513      * verifiche su anagrafica autista
061700090513     c     kaitra        chain     aitra05l
061800090513      * autista accreditato
061900090513     c                   if        %found(aitra05l)
062900090513     c                   if        adtdrc = 0
063000090513     c                   move      vdata1g       datavaliso
063100090513     c                   move      adtdts        dataconiso
063200090513     c     datavaliso    subdur    dataconiso    limconv:*d        3 0
063300090513     c                   if        limconv > limcopia
063400090513     c                   seton                                        9628
063500090513     c                   eval      $msg = 'Manca ricezione copia firmata dell''-
063600090513     c                             allegato tariffario '
063700090513     c                   leavesr
063800090513     c                   endif
063900090513     c                   endif
064000090513     c                   else
064100090513     c     adtpdr        setgt     aitra05l
064200090513     c     adtpdr        readpe    aitra05l
064300090513      * autista trovato ma disaccreditato
064400090513     c                   if        not %eof(aitra05l)
064500090513     c                   if        tradfi < vdata1g
064600090513     c                   seton                                        9628
064700090513     c                   eval      $msg = 'Autista disaccreditato il ' +
064800090513     c                             %char(%date(tradfi):*eur)
064900090513     c                   leavesr
065000090513     c                   endif
065100090513     c                   else
065200090513      * autista non trovato è da accreditare
065300090513      * verifica se è nel limite dei giorni consentiti di attesa x l'accreditamento
065400090513     c                   move      vdata1g       datavaliso
065500090513     c                   move      adtdts        dataconiso
065600090513     c     datavaliso    subdur    dataconiso    limconv:*d        3 0
065700090513     c                   if        limconv > limaccr
065800090513     c                   seton                                        9628
065900090513     c                   eval      $msg = 'Autista non accreditato procedere -
066000090513     c                             con accreditamento x valorizzarlo'
066100090513     c                   leavesr
066200090513     c                   endif
066300090513     c                   endif
066400090513     c                   endif
066401090715      * verifico se c'è corrispondenza fra partita iva fornitore e contratto
066402090715     c                   if        tranrc <> 0
066403090715     c     tranrc        chain     aitrs01l
066404090715     c                   exsr      RepIvaFor
066405090715     c                   if        trsiva <> ivaforn
066406091112     c                   move      vdata1g       datavaliso
066407091112     c                   move      adtdts        dataconiso
066408091112     c     datavaliso    subdur    dataconiso    limconv:*d        3 0
066409091112     c                   if        limconv > limaccr
066410090715     c                   seton                                        9628
066411090715     c                   eval      $msg = 'Attenzione P.IVA. fornitore -
066412090715     c                             diversa da P.IVA contratto'
066413090715     c                   leavesr
066414091112     c                   endif
066415090715     c                   endif
066416090715     c                   endif
066500081121     C                   ENDSR
066600090513      * ?___________________________________________________________________
066700090513     C     RepIvaFor     BegSR
066800090513      * ?___________________________________________________________________
066900090513     c                   clear                   ivaforn          16
067000090513     c* reperisco la p.iva del fornitore immesso in apd
067100090513     c                   move      adtcsf        socfor
067200090513     c                   move      adtcdf        codfor
067300090513     c     krco2         chain     anfrn01l
067400090513     c                   if        %found(anfrn01l)
067500090513     c     frnsogg       chain     ansog01l
067600090513     c                   if        %found(ansog01l)
067700090513     c                   movel     sogpartiva    IvaForn
067800090513     c                   end
067900090513     c                   end
068000090513      *
068100090513     C                   ENDSR
068200011227     c**********************************************************************
068300000607     C     *INZSR        BEGSR
068400011227     c**********************************************************************
068500000607      *
068600000607     C     *ENTRY        PLIST
068700000607     C                   PARM                    Kpjba
068800030604     C                   Z-ADD     1             CODUT
068900030604     C                   CALL      'X§PARUT'
069000030604     C                   PARM                    UTEDSE
069100030604     C                   MOVEL     REC80         CNCR80
069200030703     C                   MOVEL     Ragut         rsut
069300030703     C                   MOVEL     knsif         vknsif
069400030703     C                   MOVEL     knmus         vknmus
069500030702      *reperimento data
069600030702     C                   TIME                    W0120            14 0
069700030702     C                   MOVE      W0120         WDAT              8 0
069800030702     C*
069900030702     C                   Z-ADD     WDAT          G02DAT
070000030702     C                   MOVEL     *BLANK        G02ERR
070100030702     C                   CALL      'XSRDA8'
070200030702     C                   PARM                    WLBDA8
070300030702     C* UDATE A 8 IN AAAA/MM/GG
070400030717     C                   Z-ADD     G02INV        WUDAte            8 0
070500030702     C                   Z-ADD     G02INV        WUDA              8 0
070600030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
070700030617     C                   CLEAR                   Tibs36Ds
070800030617     C                   EVAL      I36ute = knmus
070900030617     C                   CALL      'TIBS36R'
071000030617     C                   PARM                    tibs36ds
071001100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
071002100617     c                   clear                   tibs02ds
071003100617     c                   clear                   dvpocont
071004100617     c                   eval      t02mod = 'C'
071005100617     c                   eval      t02sif = knsif
071006100617     c                   eval      t02cod = 'VPO'
071007100617     c                   eval      t02ke1 = 'CONT'
071008100617     c                   call      'TIBS02R'
071009100617     c                   parm                    kpjba
071010100617     c                   parm                    tibs02ds
071011100617    2c                   if        t02err = *blanks
071012100617     c                   eval      dvpocont  = t02uni
071013100617    2c                   endif
071014100318      * controllo autorizzazione del profilo
071015100617     c                   if        wuda    >= §vpoaut
071016100318     c                   movel     o36pou        i70pog
071017100318     c                   movel     o36pou        i70pge
071018100318     c                   movel     knmus         i70prf
071019100318     c                   call      'FICN70R'
071020100318     c                   parm                    ficn70ds
071021100318     c                   movel     o70uni        daut
071022100318     c                   if        §AFFCPSTD <> 'S'
071023100318     c                   exsr      uscita
071024100318     c                   endif
071025100617     c                   endif
071026100318
071100030714     c                   movel     i36ute        edp3              3
071200030908     c                   if        edp3 <> 'EDP' and o36pou <> 001
071300030714     c                   seton                                        20
071400030714     c                   endif
071500000607      *
071600030711     C     Kattd         KLIST
071700030714     C                   KFLD                    vattfgp
071800030714     C                   KFLD                    vdata1g           8 0
071900030714     C     Katt1         KLIST
072000030714     C                   KFLD                    vattfgp
072100030714     C                   KFLD                    vdata1g
072200030714     C                   KFLD                    vattprg
072300030714     C     Kapd          KLIST
072400030714     C                   KFLD                    tipoa             1
072500030722     C                   KFLD                    vattpdrk          7 0
072600030714     C                   move      'D'           tipoa
072700030725     c     krco          klist
072800090717     c                   kfld                    attsoc
072900030725     c                   kfld                    knatura           1
073000090717     c                   kfld                    attcdf
073100030725     c                   movel     'F'           knatura
073200030725
073300081121      *
073400081121     c     kadd          Klist
073500081121     c                   Kfld                    vattvad
073600081121     c                   Kfld                    vattpdrk
073700081121     c                   Kfld                    vattfgp
073800081121     c                   Kfld                    vattfga
073900081121      *
074000081121     c     kadt          Klist
074100081121     c                   Kfld                    addpdr
074200081121     c                   Kfld                    addprg
074300081121      *
074400090513     c     kaitra        klist
074500090513     c                   kfld                    adtpdr
074600090513     c                   kfld                    data0
074700090513
074800090513     c     krco2         klist
074900090513     c                   kfld                    socfor            3
075000090513     c                   kfld                    codfor            8
075100090513      *
075200090513     c     ktab          klist
075300090513     c                   kfld                    tblkut
075400090513     c                   kfld                    tblcod
075500090513     c                   kfld                    tblKEY
075600090513      *
075700030725     c                   eval      cost1 ='Io sottoscritto mi impegno ad azion-
075800030725     c                             are l''antifurto ogni volta che abbandono -
075900030725     c                             per qualsiasi motivo '
076000030725     c                   eval      cost2 ='l''automezzo e mi impegno a rendere -
076100030725     c                             immediatamente operativo l''antifurto blocca-
076200030725     c                             porte.'
076300030725     c                   eval      cost3 ='Prendo atto che sono stati applicati-
076400030725     c                              i piombi sotto annotati'
076500030725     c
076600030725     c                   eval      cost4 ='Dichiaro altresi di essere stato in-
076700040614     c                             formato ai sensi dell''art.13 D.Lgs. 196/20-
076800040614     c                             03 ed autorizzo la'
076900040614     c                   eval      cost4b='società'
077000040614     c                   eval      cost5 ='ai sensi degli art. 23 e 26 del D. -
077100040614     c                             Lgs. citato, al'
077200040614     c                   eval      cost6 = 'trattamento-
077300040614     c                              e alla comunicazione a soggetti terzi-
077400040614     c                              dei miei dati.'
077500030725     c                   move      *all'_'       cost7
077600090513     c* reperisce gg min durata allegato
077700090513     C                   clear                   ds5Aaut
077800090513     C                   movel     '5A'          tblcod
077900090513     C                   MOVEL(P)  'AUT'         tblKEY
078000090513     c                   z-add     1             tblkut
078100090513     C     KTAB          CHAIN     TABEL00F
078200090513     c                   if        %found(tabel00f)
078300090513     C                   movel     tbluni        ds5Aaut
078400090513     c                   end
078500090513
078600011227     C                   ENDSR
078700030725     C*--------------------------------------------------------------------
078800090714     O*rtf100   E            foglio           02
078900090714     o*                                          10 'FICN77R'
079000090714     o*                      knmus               22
079100090714     o*                      w0120               90 '  :  :  -  /  /    '
079200090714     O*         E            foglio           05
079300090714     o*                                       +   3 'Foglio viaggio Affluenza/-
079400090714     o*                                             Defluenza numero'
079500090714     o*                      vattprg       z  +   2
079600090714     o*                                       +   2 'Del'
079700090714     o*                      vattddc          +   2 '  /  /    '
079800090714     O*         E            foglio           07
079900090714     o*                                       +   3 'Da'
080000090714     o*                      vattfgpd         +   3
080100090714     o*                                       +   2 'a'
080200090714     o*                      vattfgad         +   3
080300090714     O*         E            foglio           09
080400090714     o*                                       +   3 'Partenza'
080500090714     o*                      vattddc          +   5 '  /  /    '
080600090714     o*                                       +   2 'ore'
080700090714     o*                      vattorpr         +   3 '  :  '
080800090714     O*         E            foglio           12
080900090714     o*                                       +   3 'Cod.Autotrasportatore:'
081000090714     o*                      vattpdr          +   1
081100090714     o*                      vattpdrd         +   5
081200090714     O*         E            foglio           14
081300090714     o*                                       +   3 'Cod.Fornitore:        '
081400090714     o*                      apdksc           +   1
081500090714     o*                      rasfor           +   5
081600090714     O*         E            foglio           18
081700090714     o*                      cost1            +   3
081800090714     O*         E            foglio           19
081900090714     o*                      cost2            +   3
082000090714     O*         E            foglio           20
082100090714     o*                      cost3            +   3
082200090714     O*         E            foglio           23
082300090714     o*                      cost4            +   3
082400090714     O*         E            foglio           24
082500090714     o*                      cost4b           +   3
082600090714     o*                      rassoc           +   1
082700090714     o*                      cost5            +   1
082800090714     O*         E            foglio           25
082900090714     o*                      cost6            +   3
083000090714     O*         E            foglio           28
083100090714     o*                                          55 'firma'
083200090714     o*                      cost7            +   3
083300090714     O*         E            foglio           30
083400090714     o*                                       +   3 'Annotazioni:'
083500090714     o*                      vattnot          +   2
083600090714     O*         E            foglio           32
083700090714     o*                                       +   3 'Piombi:'
083800090714     o*                      vattpmb          +   5
083900090714     O*         E            foglio           40
084000090714     o*                      rassoc           +   3
084100090714     o*                                       +   6 'Visto l''incaricato'
084200090714     o*                      cost7            +   1
084300090714      *---------------------------------------------------------------------
