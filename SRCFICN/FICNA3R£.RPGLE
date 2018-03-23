000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200120723     h dftactgrp(*no) actgrp(*Caller)  BNDDIR('TIBS')
000300120723     H BNDDIR('TIBS') EXTBININT(*YES)
000400120723      *----------------------------------------------------------------------------
000500080930     Fficna3d   CF   E             WORKSTN
000600080930     f                                     sfile(video2:snrr1)
000700080930     Ffiadt01l  uF a E           k disk
000800030701     Fazorg01l  iF   E           k disk
000900030718     Ffiapd01l  iF   E           k disk
001000090327     Faitra05l  iF   E           k disk
001100090327     Faitrs01l  iF   E           k disk
001200090525     Faitrs02l  iF   E           k disk    rename(aitrs000:aitrs2)
001300081009     Ffiadd01l  uF a E           k disk
001400090610     Ffiatt01l  iF   E           k disk
001500090522     Fanfrn01l  iF   E           k disk
001600090522     Fansog01l  iF   E           k disk
001700090417     Ftabel00f  iF   E           K DISK
001800111124     Ftntbe01l  iF   E           K DISK
001900011228      *
002000020103     D Psds           SDS
002100020103     D  PgmName          *PROC
002200011228     D Kpjba         E DS
002300011228      *
002400030721     D fnlv24ds      E DS
002500030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002600030604     D CNCR80        E DS
002700131003     D tibs42ds      E DS
002701131003     D uteautds      E DS
002800090114     d ficn05ds      e ds
002900090408     d ficn08ds      e ds
003000090114     d dgpi          e ds
003200120220     D ds3i          E DS
003300111124     D dvpocont      E DS
003400030617     d Tibs36Ds      e ds
003500090402     d dapdflr       e ds
003600090417     D DS5Aaut       E DS
003700120306     D DSaiats       E DS                  EXTNAME(AIATS00F)
003800011227     d data            ds            10
003900011228     d aa                      3      4
004000011228     d mm                      6      7
004100011228     d gg                      9     10
004200011228      *
004300030702     D fatto           S              1
004400120221     D soctraini       S              3
004500030610     D modalita        S              1
004600030604     D x               S              4  0
004700011227     D WrkEofS01       S              1
004800011227     D WrkCarS01       S              1
004900011228     D $VIDEO          S             10
005000030604     D snrr1           S              5i 0
005100120321      **
005200120321     d  aAAcsf         s                   like(aDTcsf)
005300120321     d  aAAcdf         s                   like(aDTcdf)
005400120321      **
005500030721     D Savkpjbu        S                   like(kpjbu)
005600080930     D SavOpzione      S                   like(v1csce)
005700090417     d isost           s               d   datfmt(*iso)
005800090417     d isodt           s               d   datfmt(*iso)
005900030606     d dataiso         s               d   datfmt(*iso)
006000030606     d dataeur         s               d   datfmt(*eur)
006100081010     d datacom         s               d   datfmt(*eur)
006200030613     D WLBDA8          DS
006300030613     D  G02DAT                 1      8  0
006400030613     D  G02INV                 9     16  0
006500030613     D  G02ERR                17     17
006600030613     D  G02TGI                18     22  0
006700080930     D param           ds
006800080930     D  parpdr                        7s 0
006900080930     D  parprg                        3s 0
007000080930     D  parmod                        1
007100120220     D partraz         DS
007200120518     D  parsoc                        3s 0
007300120502     D  pariva                       16
007400120502     D  pardtin                       8s 0
007500120502     D  pardtfi                       8s 0
007600120502     d  esito                         1
007700120502     d  errmsg                       75
007800030701      *---------------- ----------------------------------------------------
007900120723ab   **
008000120723ab   ** Prototipi.
008100120723ab    **
008200120723ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
008300120723ab    /DEFINE DFTACTGRP_NO
008400120723ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
008500120723ab    /UNDEFINE DFTACTGRP_NO
008600120723      *
008700120723ab   D TibsSof_esito...
008800120723ab   D                 S             10I 0 IMPORT
008900120723ab   D idSocieta...
009000120723EDPDCD                 S              3A
009100120723ab   **
009200120723EDPDCd dtVALIDITA      s               D
009300120723EDPDCd VALdatFINE      s               D
009400120723EDPDCd VALdatINIZ      s               D
009500030604      *---------------- Gestione formato video video01----------------------
009600120723ab   C*     Inizializza
009700120723ab    /FREE
009800120723ab     TibsSof_Init();
009900120723ab    /END-FREE
009901131014     c                   if        o36pou <> 046
010000131014     C                   EXSR      Inzvideo1
010001131014     c                   endif
010100030604     C                   DOU       $Video <> 'VIDEO1'
010200000608      *----------- Visualizzo dati di output in caso di errori.-------------
010300000608     C                   IF        *IN99
010400000608     C                   EVAL      *IN99 = *OFF
010500030604     C                   WRITE     video1
010600000608     C                   EVAL      *IN99 = *ON
010700000608     C                   ENDIF
010800000608      *
010900030702     c     emetti        tag
011000030604     C                   EXFMT     video1
011100000608      *
011200011227     C                   SELECT
011300030612     C                   WHEN      *INKC
011400011227     C                             OR
011500011227     C                             *INKL
011600011227     C                   EVAL      $Video = *BLANKS
011700011227
011800011227     C                   WHEN      *INKE
011900030604     C                   EXSR      Inzvideo1
012000011227     C                   WHEN      *INKB
012100030604     C                   EXSR      Chkvideo1
012200011227     C                   OTHER
012300030604     C                   EXSR      Chkvideo1
012400040728     c   84              goto      emetti
012500030604     C  N99              EXSR      Wrkvideo2c
012600011227     C                   ENDSL
012700011227      *
012800011227     C                   ENDDO
012900011227      *
013000011227     C                   EXSR      Uscita
013100011227     c**********************************************************************
013200011227     c* uscita
013300011227     c**********************************************************************
013400011227     C     Uscita        BEGSR
013500011227      *
013600011227     C                   EVAL      *INLR = *ON
013700011227     C                   RETURN
013800011227      *
013900011227     C                   ENDSR
014000011227     c**********************************************************************
014100011227     c* gestione sfl
014200011227     c**********************************************************************
014300030604     C     Wrkvideo2c    BEGSR
014400000627      *
014500011227     C                   EVAL      WrkCarS01 = *ON
014600030604     C                   EVAL      $Video = 'VIDEO2C'
014700011227      *
014800030604     C                   DOU       $Video <> 'VIDEO2C'
014900030604     C                   EXSR      Carvideo2
015000011228     c*Descrizione della ricerca
015100000627      *
015200030604     C                   WRITE     video2z
015300030604     C                   EXFMT     video2c
015400090429     c                   setoff                                       2896
015500090429     c                   clear                   $msg
015600011227     C                   SELECT
015700011228     c* fine
015800011227     C                   WHEN      *INKC=*ON
015900011227     C                   EVAL      $VIDEO=*BLANKS
016000011228     c* guida
016100011227     C                   WHEN      *INKL=*ON
016200030604     C                   EVAL      $VIDEO='VIDEO1'
016300011228     c* ripristino
016400011227     C                   WHEN      *INKE=*ON
016500011227     C                   EVAL      WrkCarS01 = *ON
016600011227
016700011227     C                   OTHER
016800011227
016900011227     C                   EXSR      GestioneSFL
017000011227     C                   ENDSL
017100011228
017200011227     C                   ENDDO
017300031027     C                   ENDSR
017400011227     c**********************************************************************
017500011227     c* Controlli video R01.
017600011227     c**********************************************************************
017700030604     C     Chkvideo1     BEGSR
017800000609      *
017900000608     C                   EVAL      *IN99 = *OFF
018000040728     C                   EVAL      *IN84 = *OFF
018100081009     C                   EVAL      *IN40 = *OFF
018200080930     c     vidfil        chain     azorg01l
018300080930     c                   movel     orgdes        vidfild
018301130808      * controllo autorizzazione del profilo ad accedere alle tariffe AFF/DEF
018303131003     c                   clear                   tibs42ds
018304131003     c                   movel     vidfil        i42pge
018306131003     c                   call      'TIBS42R'
018307131003     c                   parm                    tibs42ds
018308131003     c                   movel     o42uni        uteautds
018309130808     c                   if        §affmtc <> 'S'
018310130808     c                   exsr      uscita
018311130808     c                   endif
018400081013      *modalità interrogazione proteggo i campi e passo ai chiamati opzione interrogazione
018500081013     c                   if        kcdaz = 'FNA4'
018600081013     c                   seton                                        2125
018700081013     c                   move      'I'           parmod
018800081013     c                   endif
018900080930      *
019000080930     C****  CODICE PADRONCINO  ****
019100080930     C     '?'           SCAN      vidpdr                                 55
019200080930     C*
019300080930     C     *IN55         IFEQ      *ON
019400080930     C                   MOVEL     KPJBU         SAVKPJBU
019500080930     C                   Z-ADD     Vidfil        d24FIL
019600080930     C                   Z-ADD     0             d24pdr
019700080930     C                   MOVEL     'R'           d24FLG
019800080930     C                   MOVEL     'D'           d24tip
019900080930     C                   MOVEL     *BLANKS       d24rsc
020000080930     C                   MOVEL(p)  fnlv24ds      KPJBU
020100080930     C                   CALL      'FNLV24R'
020200080930     C                   PARM                    KPJBA
020300080930     C                   MOVEL     KPJBU         fnlv24ds
020400080930     C                   MOVEL     savKPJBU      KPJBU
020500080930     C*
020600080930     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
020700080930     C     d24pdr        IFNE      0
020800080930     C                   MOVEL     d24pdr        Vidfil
020900080930     C                   MOVE      d24pdr        Vidpdr
021000080930     C                   MOVEL     d24rsc        despdr
021100080930     c                   else
021200080930     C                   clear                   Vidpdr
021300080930     C                   clear                   Vadtpdrk
021400080930     C                   clear                   despdr
021500081009     c                   seton                                        4084
021600080930     C                   ENDIF
021700080930     C*
021800080930     C                   ENDIF
021900080930      * decodifica autista
022000081009     c                   if        vidfil = 0 or vidpdr = *zeros
022100081009     c                             or vidpdr = *blank
022200081009     c                   seton                                        4084
022300081009     c                   endif
022400080930     c                   if        vidfil  <> *zeros and vidpdr <> *blank
022500080930     c                             and vidpdr > '0000'
022600080930     c                   movel     vidfil        vadtpdrk
022700080930     c                   move      vidpdr        vadtpdrk
022800081001     c                   clear                   despdr
022900081001     c     kapd          chain     fiapd01l
023000090720     c                   if        %found(fiapd01l)
023100090720     c                   if        apdatb = *blank or
023200090720     c                             apdatb <>*blank and parmod ='I'
023300090507     c                   movel     apdrsf        despdr
023400081001     c                   else
023500081009     c                   seton                                        4084
023600081001     c                   endif
023700091109     c                   else
023800091109     c                   seton                                        4084
023900090720     c                   endif
024000080930     c                   endif
024100000608      *
024200000608     C                   ENDSR
024300011227     c**********************************************************************
024400011227     c* inizializza r01
024500011227     c**********************************************************************
024600030604     C     Inzvideo1     BEGSR
024700000608
024800080930     c                   move      o36pou        vidfil
024900030604     C                   EXSR      Chkvideo1
025000000609
025100000608     C                   ENDSR
025200030604     c**********************************************************************
025300030605     C     opzione       BEGSR
025400030605     c**********************************************************************
025500030610     c                   move      v1csce        modalita
025600040728     c                   movea     '000000000000'*in(40)
025700040728     c                   movea     '00000'       *in(52)
025800081010     c* se non è ultimo progressivo proteggo la data scadenza in quanto
025900081010     c* si andrebbe a creare un buco temporale fra data scadenza e data decorrenza
026000081010     c* del progressivo precedente nel caso in cui venisse modificata
026100081010     c                   seton                                        23
026200081010     c                   setoff                                       22
026300081010     c                   if        vi2prg < lastprg
026400081010     c                   seton                                        22
026500081010     c                   endif
026600040728     c                   setoff                                       96
026700030605     c                   select
026800080930     c                   when      v1csce = 'D'
026900080930     c                   exsr      dettagli
027000081013     c                   when      v1csce = '2' and not *in21
027100030605     c                   exsr      modifica
027200090608     c                   when      v1csce = '3' and not *in21  or
027300090608     c                             v1csce = '9' and not *in21
027400090508     c                   exsr      copia
027500090324     c                   when      v1csce = '7'
027600090324     c                             and not *in21
027700090324     c                   exsr      modifica
027800090324     c                   when      v1csce = '8'
027900090324     c                             and not *in21
028000090324     c                   exsr      stampa
028100090324     c                   exsr      modifica
028200030605     c                   when      v1csce = '5'
028300030606     c                   seton                                        21
028400030606     c                   exsr      modifica
028500081013      *non spengo indicatore se sono in interrogazione
028600081013     c                   if        kcdaz <> 'FNA4'
028700030606     c                   setoff                                       21
028800081013     c                   endif
028900081013     c                   when      v1csce = '4' and not *in21
029000090609     c*                  seton                                        21
029100040728     c                   exsr      modifica
029200040728     c                   setoff                                       2128
029300040728     c                   clear                   $msg
029400030605     c                   endsl
029500030604     C                   ENDSR
029600030605     c**********************************************************************
029700030605     C     modifica      BEGSR
029800030605     c**********************************************************************
029900090522     c                   if        modalita = '5'
030000090522     c     kadt          chain(n)  fiadt01l                           98
030100090522     c                   else
030200090522     c     kadt          chain     fiadt01l                           98
030300090522     c                   endif
030400030610     c                   if        not *in98
030500080930     c                   move      ADTPDR        vADTPDR
030600080930     c                   move      ADTPRG        vADTPRG
030700080930     c                   move      ADTDDT        vADTDDT
030800080930     c                   move      ADTDST        vADTDST
030900080930     c                   move      ADTDCN        vADTDCN
031000080930     c                   move      ADTDTS        vADTDTS
031100080930     c                   move      ADTDUV        vADTDUV
031200111205     c                   z-add     ADTpcar       vADTPcar
031300120104     c     adttsr        comp      'R'                                    20
031400111214     c                   seton                                        88
031500090402     c                   z-add     adtnrc        vadtnrc
031600090402     c                   z-add     adtfil        vadtfil
031700090402     c                   z-add     adtpkl        vadtpkl
031800090408     c                   z-add     adttmpc       vadttmpc
031900090408     c                   z-add     adttmps       vadttmps
032000090428     c                   move      adtcdf        vADTcdf
032100090428     c                   move      adtcsf        vADTcsf
032200090522     c                   clear                   vdesforn
032300090522     c     kfrn          chain     anfrn01l
032400090522     c                   if        %found(anfrn01l)
032500090522     c                   movel     frndesbrev    vdesforn
032600090522     c                   endif
032700110928     c                   if        modalita ='4' and vadtdts > 0
032800110928     c                   if        %subst(knmus:1:3) <> 'EDP'
032900081009     c                   eval      $msg = 'Tariffa non cancellabile -
033000090324     c                             già convalidata'
033100090609     c                   seton                                        289621
033200090609     c                   except    libera
033300110928     c                   else
033400110928      * controllo che la data scadenza  non sia inferiore ad eventuali valorizzazioni
033500110928     c     katt          setgt     fiatt01l
033600110928     c                   do        *hival
033700110928     c     vidfil        reade     fiatt01l
033800110928     c                   if        %eof(fiatt01l)
033900110928     c                   leave
034000110928     c                   endif
034100110928     c                   if        attpdr = vadtpdr  and
034200110928     c                             attddc >= adtddt and attddc <= adtdst
034300110928     c                   eval      $msg = 'Tariffa non cancellabile -
034400110928     c                             esistono valorizzazioni per l''autista'
034500110928     c                   seton                                        289621
034600110928     c                   except    libera
034700110928     c                   endif
034800110928     c                   enddo
034900110928     c*
035000110928     c                   endif
035100081009     c                   endif
035200090324     c                   if        modalita ='7' and vadtdts > 0
035300090324     c                   eval      $msg = 'Tariffa -
035400090324     c                             già convalidata'
035500090324     c                   seton                                        2896
035600090525     c                   endif
035700090324     c                   if        modalita ='8'
035800090324     c                   eval      $msg = 'Tariffa stampata'
035900090609     c                   seton                                        289621
036000090325     c                   except    libera
036100090324     c                   endif
036200030718      * decodifica autista
036300081001     c                   clear                   vadtpdrd
036400081001     c     kapd          chain     fiapd01l
036500090720     c                   if        %found(fiapd01l)
036600090507     c                   movel     apdrsf        vadtpdrd
036700081001     c                   endif
036800090408      * se non presente imposta peso da anagrafica gestionale
036900090417     c                   if        adttmpc = 0 or adttmps = 0
037000090417     c                   clear                   ficn08ds
037100090417     c                   move      'D'           i08tip
037200090417     c                   z-add     adtddt        i08data
037300090417     c                   z-add     vadtpkl       i08mcp
037400090417     c                   call      'FICN08R'
037500090417     C                   parm                    ficn08ds
037600090417     c                   if        o08err = *blank
037700090417     c                   z-add     o08tc         vadttmpc
037800090417     c                   z-add     o08ts         vadttmps
037900090417     c                   else
038000090408     c                   clear                   vadttmpc
038100090408     c                   clear                   vadttmps
038200090408     c                   endif
038300090408     c                   endif
038400030701      *data decorrenza
038500080930     c                   if        adtddt > 0
038600080930     c                   move      adtddt        dataiso
038700030701     c                   move      dataiso       dataeur
038800080930     c                   move      dataeur       vadtddt
038900030701     c                   else
039000080930     c                   clear                   vadtddt
039100030701     c                   endif
039200030701      *data scadenza
039300080930     c                   if        adtdst > 0
039400080930     c                   move      adtdst        dataiso
039500030606     c                   move      dataiso       dataeur
039600080930     c                   move      dataeur       vadtdst
039700030630     c                   else
039800080930     c                   clear                   vadtdst
039900030606     c                   endif
040000080930     c                   if        adtdcn > 0
040100080930     c                   move      adtdcn        dataiso
040200080930     c                   move      dataiso       dataeur
040300080930     c                   move      dataeur       vadtdcn
040400080930     c                   else
040500080930     c                   clear                   vadtdcn
040600080930     c                   endif
040700120104     c* ristampa
040800120104     c* se è una ristampa si deve riempire la "data di stampa" con
040900120104     c* la 1° data certa, e mettere nella "data di ristampa"
041000120104     c* l'ultima data certa ...
041100120104     c                   if        adttsr = 'R'
041200120104     c                   if        adtunodc > 0
041300120104     c                   move      adtunodc      dataiso
041400120104     c                   move      dataiso       dataeur
041500120104     c                   move      dataeur       vadtunodc
041600120104     c                   else
041700120104     c                   clear                   vadtunodc
041800120104     c                   endif
041900120104     c                   eval      vadtunodc = vadtdcn
042000120104     c                   if        adtunodc <> 0
042100120104     c                   move      dataeur       vadtdcn
042200120104     c                   else
042300120104     c* tariffe prima di dicembre 2011 quando non esisteva ancora
042400120104     c* il campo della prima data certa
042500120104     c                   clear                   vadtdcn
042600120104     c                   end
042700120104     c                   end
042800120104     c*
042900080930     c                   if        adtdts > 0
043000080930     c                   move      adtdts        dataiso
043100080930     c                   move      dataiso       dataeur
043200080930     c                   move      dataeur       vadtdts
043300080930     c                   else
043400080930     c                   clear                   vadtdts
043500080930     c                   endif
043600090324     c                   if        adtdrc > 0
043700090324     c                   move      adtdrc        dataiso
043800090324     c                   move      dataiso       dataeur
043900090324     c                   move      dataeur       vadtdrc
044000090324     c                   else
044100090324     c                   clear                   vadtdrc
044200090324     c                   endif
044300080930     c                   if        adtduv > 0
044400080930     c                   move      adtduv        dataiso
044500080930     c                   move      dataiso       dataeur
044600080930     c                   move      dataeur       vadtduv
044700080930     c                   else
044800080930     c                   clear                   vadtduv
044900080930     c                   endif
045000030605     c                   exsr      Gesvideo3
045100030605     c                   endif
045200030605     C                   ENDSR
045300030605     c**********************************************************************
045400030605     C     copia         BEGSR
045500030605     c**********************************************************************
045600090508     c                   clear                   video3
045601131021     c                   move      knmus         vknmus
045602131021     c                   move      knsif         vknsif
045603131021     c                   movel     ragut         rsut
045700090508     c                   seton                                        23
045800090508     c     Vadtpdrk      setgt     fiadt01l
045900090508     c     Vadtpdrk      readpe(n) fiadt01l
046000090508     c                   if        adtdts = 0
046100090508     c                   eval      $msg = 'PROGRESSIVO PRECEDENTE NO-
046200090508     c                             N CONVALIDATO INSERIMENTO NON AMMESSO'
046300090508     c                   seton                                        2896
046400090508     C                   leavesr
046500090508     c                   endif
046600090508     c                   move      ADTPDR        vADTPDR
046700090508     c                   eval      vADTPRG =  ADTPRG + 1
046800090508     c                   move      adtdst        dataiso
046900090508     c                   move      dataiso       datacom
047000090508     c     datacom       adddur    1:*d          datacom
047100090508     c                   move      datacom       vadtddt
047200080930      * decodifica autista
047300090508     c                   clear                   vadtpdrd
047400090508     c     kapd          chain     fiapd01l
047500090720     c                   if        %found(fiapd01l)
047600090508     c                   movel     apdrsf        vadtpdrd
047700090508     c                   endif
047800090508     c                   clear                   fiadt000
047900090525     c                   clear                   vADTDST
048000090525     c                   clear                   vADTDCN
048100120104     c                   clear                   vADTunodc
048200090525     c                   clear                   vADTDTS
048300090525     c                   clear                   vADTDUV
048400090508     c                   exsr      Gesvideo3
048500030605     C                   ENDSR
048600090324     c**********************************************************************
048700090324     C     stampa        BEGSR
048800090324     c**********************************************************************
048900090324     c                   clear                   param
049000090324     c                   z-add     vadtpdrk      parpdr
049100090324     c                   z-add     vi2prg        parprg
049200090324     c                   movel     param         kpjbu
049300090324     c                   call      'FICNA6R1'
049400090324     c                   parm                    kpjba
049500090324     c
049600090324     C                   ENDSR
049700081007     c**********************************************************************
049800081007     C     copiadettaglioBEGSR
049900081007     c**********************************************************************
050000081007     c                   clear                   video3
050001131021     c                   move      knmus         vknmus
050002131021     c                   move      knsif         vknsif
050003131021     c                   movel     ragut         rsut
050100081007     c     kadt          setll     fiadd01l
050200081007     c                   do        *hival
050300081007     c     kadt          reade     fiadd01l
050400081007     c                   if        %eof(fiadd01l)
050500081007     c                   leave
050600081007     c                   endif
050700081007     c                   if        addatb <> *blank
050800081007     c                   iter
050900081007     c                   endif
051000081007
051100081007     c                   z-add     ADTPDR        ADdPDR
051200081007     c                   z-add     ADTPRG        addprg
051300081007     c                   move      ADDTSR        addTSR
051400081007     c                   z-add     ADDLNP        addLNP
051500081007     c                   z-add     ADDLNA        addLNA
051600081007     c                   z-add     ADDIMP        addIMP
051700081007     c                   move      ADDDIV        addDIV
051800081007     c                   move      ADDNOTE       addNOTE
051900081007     c                   z-add     ADDKM         addKM
052000081007     c                   write     fiadd000
052100081007     c                   enddo
052200081007     C                   ENDSR
052300090703     c**********************************************************************
052400090703     C     annuldettaglioBEGSR
052500090703     c**********************************************************************
052600090703     c                   clear                   video3
052601131021     c                   move      knmus         vknmus
052602131021     c                   move      knsif         vknsif
052603131021     c                   movel     ragut         rsut
052700090703     c     kadt          setll     fiadd01l
052800090703     c                   do        *hival
052900090703     c     kadt          reade     fiadd01l
053000090703     c                   if        %eof(fiadd01l)
053100090703     c                   leave
053200090703     c                   endif
053300090703     c                   move      'A'           addatb
053400090703     c                   update    fiadd000
053500090703     c                   enddo
053600090703     C                   ENDSR
053700080930     c**********************************************************************
053800080930     C     dettagli      BEGSR
053900080930     c**********************************************************************
054000080930     c                   clear                   param
054100080930
054200081013     c                   if        kcdaz  = 'FNA4'
054300081013     c                   move      'I'           parmod
054400081013     c                   else
054500080930     c                   clear                   parmod
054600080930     c                   if        vi2dts > 0
054700080930     c                   move      '5'           parmod
054800080930     C                   endif
054900081013     C                   endif
055000080930     c                   z-add     vadtpdrk      parpdr
055100080930     c                   z-add     vi2prg        parprg
055200080930     c                   movel     param         kpjbu
055300080930     c                   call      'FICNA4R'
055400080930     C                   PARM                    KPJBA
055500080930     C
055600080930     c                   endsr
055700030605     c**********************************************************************
055800030605     C     gesvideo3     BEGSR
055900030605     c**********************************************************************
056000030702     c                   clear                   fatto
056100120307     c                   clear                   forzato           1
056200120307     c                   setoff                                       29
056300090324     c                   do        *hival
056400030605     c                   exfmt     video3
056500081001     c                   setoff                                       2896
056600120307      *
056700120307      *** Forza con il comando F11 il controllo di inesistenza
056800120307      *    dell'autista tra i contratti dei Trazionisti.
056900120307     c   29              if        *inKk
057000120307     c                   eval       forzato = 'S'
057100120307     c                   setoff                                       29
057200120307     c                   end
057300120307     c
057400081001     c                   clear                   $msg
057500030605     c   kl              leave
057600040728     c  n21              exsr      controv3
057700030624     c   96              iter
057800090609     c                   if        not *in21
057900040728     c   kf              exsr      aggiorna
058000090609     c                   endif
058100030702     c                   if        fatto <> *blank
058200030702     c                   leave
058300030702     c                   endif
058400030605
058500030605     c                   enddo
058600030605     C                   ENDSR
058700030605     c**********************************************************************
058800030624     C     Controv3      BEGSR
058900030605     c**********************************************************************
059000030613      *
059100030702      * *in96 errore generico videata dettaglio
059200040728     c                   setoff                                       9628
059300040728     c                   clear                   $msg
059400040728     c                   if        modalita ='4'
059500040728     c                   goto      endctr
059600040728     c                   endif
059700120502      *controlli data  decorrenza
059800120502     c                   setoff                                       505253
059900120502     c                   if        vadtddt > 0
060000120502     C                   MOVE      Vadtddt       G02DAT
060100120502     C                   MOVEL     *BLANK        G02ERR
060200120502     C                   CALL      'XSRDA8'
060300120502     C                   parm                    wlbda8
060400120502     C     G02ERR        IFEQ      '1'
060500120502     C                   SETON                                        5096
060600120502     c                   leavesr
060700120502     C                   END
060800120502     c                   move      g02dat        vadtddt
060900120502     c                   move      g02inv        vadtddtg          8 0
061000120502     c                   endif
061100120502      *data scadenza obbligatoria
061200120502     c                   setoff                                       54
061300120502     c                   if        vadtdst =  *zeros
061400120502     c                   seton                                        5496
061500120502     c                   leavesr
061600120502     c                   endif
061700120502      *controlli data  scadenza
061800120502     c                   setoff                                       51
061900120502     c                   if        vadtdst > 0
062000120502     C                   MOVE      Vadtdst       G02DAT
062100120502     C                   MOVEL     *BLANK        G02ERR
062200120502     C                   CALL      'XSRDA8'
062300120502     C                   PARM                    WLBDA8
062400120502     C     G02ERR        IFEQ      '1'
062500120502     C                   SETON                                        5196
062600120502     c                   leavesr
062700120502     C                   END
062800120502     c                   move      g02dat        vadtdst
062900120502     c                   move      g02inv        vadtdstg          8 0
063000120502     c                   endif
063100120308     c                   clear                   flg_TRAZattiv     1
063200120308      **
063300120308      **   per prima cosa deve sapere se si tratta di un TRAZIONISTA ATTIVO
063400120308      ***  oppure NO
063500120308      *               *-----------------------*
063600120502     c                   clear                   partraz
063700120502     c                   z-add     vadtddtg      pardtin
063800120502     c                   z-add     vadtdstg      pardtfi
063900120308     c                   exsr      veriftraz
064000120308      *               *-----------------------*
064100120717     c                   if        esito <> ' '
064200120308     c                   eval      flg_TRAZattiv='S'
064300120308      **
064400120308      ***  Se TRAZIONISTA Accreditato:
064500120308      ***  La società immessa non coincide con quella dei TRAZIONISTI
064600120308      ***   ma.....
064700120308      ***    Questo può essere Forzabile per poter proseguire.
064800120308     c                   if        apdcsf <> *blank and apdcsf <> soctraini
064900120308      *
065000120308      * se forza con F11 NON deve + emettere questo controllo
065100120605     c                   if         forzato <> *blank
065200120308     c                   eval      $msg = 'Se Fornitore fa TRAINI STANDARD,-
065300120308     c                              la società deve essere '
065400120308     c                              + soctraini + '. F11 x SOC.diversa'
065500120308     c                   seton                                        2896
065600120308     c                   seton                                        29
065700120308     c                   leavesr
065800120308     c                   end
065900120308      ***
066000120308     c                   endif
066100120502      *** gestione errore in verifica trazionista
066200120717     c*m                 else
066300120717     c*m                 if        esito = '1'
066400120717     c*m                 eval      $msg = errmsg
066500120717     c*m                 seton                                        2896
066600120717     c*m                 leavesr
066700120717     c*m                 endif
066800120308     c                   endif
066900120724      *verifica congruenza data scadenza tariffa con società operativa corretta
067000130110     c                   if        modalita ='2' and vadtdst > 0 and esito = ' '
067100121219     c                             and adtcsf <> ' '
067200130110     c********************         vadtdst <> 31122039
067300130110     c**  Tolto il diverso da infinito perchè avendo spostato il controllo della
067400130110     c**  società basato sulla data scadenza e non decorrenza NON deve esserci più,
067500130110     c**  altrimenti, ciò permetterebbe di riaprire una tariffa fatta scadere sulla
067600130110     c**  precedente società, riattivandola con scadenza 31122039 e il controllo
067700130110     c**  verrebbe saltato. Invece deve essere controllata la Società Operativa!!
067800120724     c                   movel     adtpdr        adtpdrf           3 0
067900121219     c                   move      vadtdstg      dtvalidita
068000120724ab   C                   exsr      TEST_societa
068100121219     c                   if        idsocieta <> adtCSF
068200121218      *
068300120724ab   c                   seton                                        2896
068400120724     c                   eval      $msg = 'Attenzione data scadenza -
068500121219     c                             incongruente alla data validità -
068600120724     c                             della società operativa'
068700120724     c                   leavesr
068800120724ab   C                   end
068900120724ab   C                   end
069000120306     ***
069100120724     ***  Solo se in convalida della tariffa:
069200090525     c                   if        modalita ='7' and adtdts = 0
069300120723      *test società
069400120723     c                   if        esito = ' '
069500120723     c                   movel     adtpdr        adtpdrf           3 0
069600121219     c                   move      adtddt        dtvalidita
069700120725ab   C                   exsr      TEST_societa
069800120723ab   c                   if        apdcsf <> idsocieta
069900120723ab   c                   seton                                        2896
070000120723     c                   eval      $msg = 'Tariffa non Convalidabile -
070100120723     c                             società fornitore non congruente -
070200120723     c                             alla società operativa'
070300120723     c                   leavesr
070400120723ab   C                   end
070500120724     c                   move      adtdst        dataiso
070600120724ab   c                   if        adtdst > valdatfinew
070700120724ab   c                   seton                                        2896
070800120724     c                   eval      $msg = 'Scadenza tariffa maggiore -
070900120724     c                             di data scadenza della società -
071000120724     c                             operativa'
071100120724     c                   leavesr
071200120724ab   C                   end
071300120723ab   C                   end
071400120723      *
071500090525     c                   clear                   sidet
071600090525     c     kadt          setll     fiadd01l
071700090525     c                   do        *hival
071800090525     c     kadt          reade(n)  fiadd01l
071900090525     c                   if        %eof(fiadd01l)
072000090525     c                   leave
072100090525     c                   endif
072200090525     c                   if        addatb = *blank
072300090525     c                   move      'X'           sidet             1
072400090525     c                   leave
072500090525     c                   endif
072600090525     c                   enddo
072700090525     c                   if        sidet = *blank
072800090525     c                   eval      $msg = 'Tariffa non Convalidabile -
072900090525     c                             manca dettaglio'
073000090525     c                   seton                                        2896
073100090525     c                   leavesr
073200090525     c                   endif
073300120220      *controllo se trazionista il fornitore deve essere in società TRAINI
073400120321     c                   if        apdcsf <> *blank and apdcsf <> soctraini
073500120307      ***
073600120308      ***+++XX sono state portate fuori dalla IF che lo eseguiva solo in Conferma Tariffa
073700120321     c                   move      apdcsf        adtcsf
073800120321     C                   move      *zeros        adtcdf
073900120321     C                   move      apdksc        adtcdf
074000120321     c                   endif
074100120307      ***
074200120221     c                   endif
074300120308      *** ---------------------------------------------
074400090327      * controllo che la data inserimento non sia inferiore al 01/05/2009
074500090325      * data partenza procedura
074600090325     c                   if        vadtddtg < 20090501
074700090114     c                   seton                                        5096
074800090417     c                   leavesr
074900090114     c                   endif
075000111124     c* le supertestate devono essere inserite solo se non presente
075100111124     c* la data di blocco impostata nella tabella VPO record CONT
075200111124     c                   if        vadtddtg >= §vpodpa
075300111124     C                   SETON                                        5096
075400111124     C                   leavesr
075500111124     c                   end
075600120308      ** ------------------------------------------------
075700090327      * verifico se esiste un contratto e se esiste la data decorrenza non può essere
075800090327      * inferiore alla data decorrenza del contratto
075900120308      ***
076000120308     c                   if        flg_TRAZattiv <>'S' or forzato ='S'
076100120308      ***
076200090327     c     kaitra        chain     aitra05l
076300090327     c                   if        %found(aitra05l)
076400090611     c                   if        tradin > vadtddtg
076500090611     c                   seton                                        5396
076600090611     c                   leavesr
076700090611     c                   endif
076800090327     c     tranrc        chain     aitrs01l
076900090327     c                   if        %found(aitrs01l)
077000090327     c                   if        trsdec > vadtddtg
077100090611     c                   seton                                        5296
077200090417     c                   leavesr
077300090327     c                   endif
077400090327     c                   endif
077500090525     c                   else
077600090525      * se non è accreditato provo con il fornitore se c'è
077700090525     c                   if        adtcdf <> *blank
077800090525      *prima di impostare data convalida verifica se esiste contratto congruente
077900090525     c                   exsr      srcdf
078000090525     c                   endif
078100090327     c                   endif
078200120308      *---
078300120308     c                   endif
078400120308      *------------
078500090114      * controllo che la data scadenza  non sia maggiore del 31/12/2039
078600090114     c                   if        vadtdstg > 20391231
078700090114     c                   seton                                        5196
078800090417     c                   leavesr
078900090114     c                   endif
079000120306      ***
079100090610      * controllo che la data scadenza  non sia inferiore ad eventuali valorizzazioni
079200090610     c                   setoff                                       57
079300090610     c                   if        vadtdstg > 0 and vadtdstg <> 20391231
079400090610     c     katt          setgt     fiatt01l
079500090610     c                   do        *hival
079600090610     c     vidfil        reade     fiatt01l
079700090610     c                   if        %eof(fiatt01l)
079800090610     c                   leave
079900090610     c                   endif
080000090610     c                   if        attpdr = vadtpdr
080100090610     c                   seton                                        5796
080200090610     c                   leavesr
080300090610     c                   endif
080400090610     c                   enddo
080500090610     c                   endif
080600090417      * controllo che la data scadenza  non sia inferiore al numero di giorni imposti
080700090417      * per la durata minima della tariffa
080800090417     c                   setoff                                       56
080900090417     c                   if        vadtdstg > 0 and vadtddtg > 0
081000090417     c                   move      vadtdstg      isost
081100090417     c                   move      vadtddtg      isodt
081200090417     c     isost         subdur    isodt         ggvalidi:*d       4 0
081300090417     c                   if        ggvalidi < giomin
081400090417     c                   seton                                        5696
081500090417     c                   leavesr
081600090417     c                   endif
081700090417     c                   endif
081800030702      *controllo data decorrenza maggiore data scadenza
081900030702     c                   setoff                                       47
082000080930     c                   if        vadtddtg > vadtdstg
082100030702     c                   seton                                        4796
082200090417     c                   leavesr
082300030702     c                   endif
082400090114     c                   if        vadtpcar = 0
082500090114     c                   clear                   ficn05ds
082600090114     c                   call      'FICN05R'
082700090114     c                   parm                    ficn05ds
082800090114     c                   eval      dgpi = o05gpi
082900090114     c                   z-add     D§GPIGPI      vadtpcar
083000120104     c     adttsr        comp      'R'                                    20
083100111214     c                   seton                                        88
083200090114     c                   endif
083300040728     C     endctr        ENDSR
083400030605     c**********************************************************************
083500030605     C     aggiorna      BEGSR
083600030605     c**********************************************************************
083700080930     c                   move      vADTPDR       ADTPDR
083800080930     c                   move      vADTPRG       ADTPRG
083900111205     c                   z-add     vADTpcar      ADTPcar
084000090402     c                   z-add     vADTpkl       ADTPkl
084100090408     c                   z-add     vADTtmpc      ADTtmpc
084200090408     c                   z-add     vADTtmps      ADTtmps
084300080930     c                   if        vadtddt > 0
084400080930     c                   move      vadtddt       dataeur
084500030610     c                   move      dataeur       dataiso
084600080930     c                   move      dataiso       adtddt
084700030610     c                   endif
084800080930     c                   if        vadtdst > 0
084900080930     c                   move      vadtdst       dataeur
085000030610     c                   move      dataeur       dataiso
085100080930     c                   move      dataiso       adtdst
085200030610     c                   endif
085300081002     c                   z-add     wuda          adtduv
085400090324      * convalida tariffa
085500090324     c                   if        modalita = '7' and adtdts = 0
085600090324     c                   z-add     wuda          adtdts
085700090327      * imposta fornitore da anagrafica gestionale
085800090427     c                   move      *zeros        adtcdf
085900090427     c                   move      apdksc        adtcdf
086000090327     c                   eval      adtcsf = apdcsf
086100120220      * se al momento della convalida verifico che il fornitore è un trazionista
086200120220      * forzo tutti 9 nel contratto e nella filiale gestione per riconoscere che non
086300120220      * necessita di accreditamento da parte della automezzi e può essere stampato
086400120502     c                   clear                   partraz
086500120502      * inseriti anche qui i parametri per controllo delle date per ora asteriscati
086600120502      * da verificare se qui sono necessari essendo il controllo completo attivo nella
086700120502      * routine di controllo qui può bastare sapere solo se è o non è trazionista
086800120518     c                   z-add     adtddt        pardtin
086900120518     c                   z-add     adtddt        pardtin
087000120220     c                   exsr      veriftraz
087100120220     c                   endif
087200090324      * inserimento e copia scrive altrimenti aggiorna il record esistente
087300040728     c                   select
087400040728     c                   when      modalita = '4'
087500081009      *elimina dettaglio
087600081009     c     kadt          setll     fiadd01l
087700081009     c                   do        *hival
087800081009     c     kadt          reade     fiadd01l
087900081009     c                   if        %eof(fiadd01l)
088000081009     c                   leave
088100081009     c                   endif
088200081009     c                   delete    fiadd000
088300081009     c                   enddo
088400080930     c                   delete    fiadt000
088500081009
088600040728     c                   when      modalita = *blank or modalita = '3'
088700081007     c                             or modalita = '9'
088800080930     c                   write     fiadt000
088900081007      *
089000081007     c                   if        modalita = '9'
089100081007     c                   exsr      copiadettaglio
089200081007     c                   endif
089300090324      *
089400040728     c                   other
089500080930     c                   update    fiadt000
089600040728     c                   endsl
089700030702     c                   move      'X'           fatto
089800030605     C                   ENDSR
089900011228     c**********************************************************************
090000011228     c* carica sfl
090100011228     c**********************************************************************
090200030604     C     Carvideo2     BEGSR
090300011228     C                   IF        WrkCarS01 = *ON
090400030604     C                   EVAL      snrr1   = 0
090500011228     C                   EVAL      *IN90 = *ON
090600020219     C                   EVAL      *IN91 = *OFF
090700030604     C                   WRITE     video2c
090800011228     C                   EVAL      *IN90 = *OFF
090900080930     c                   movel     vidfil        vadtpdrk
091000080930     c                   move      vidpdr        vadtpdrk
091100080930     c     vadtpdrk      setll     fiadt01l
091200030701     c                   do        *hival
091300080930     c     vadtpdrk      reade     fiadt01l                               97
091400030701     c   97              leave
091500080930     c                   if        adtatb <> *blank
091600080930     c                   iter
091700080930     c                   endif
091800030701     c                   move      *blanks       v1csce
091900080930     c                   z-add     adtprg        vi2prg
092000081010     c                   z-add     adtprg        lastprg           3 0
092100030701      *data decorrenza
092200080930     c                   if        adtddt > 0
092300080930     c                   move      adtddt        dataiso
092400030701     c                   move      dataiso       dataeur
092500080930     c                   move      dataeur       vi2ddt
092600030701     c                   else
092700080930     c                   clear                   vi2ddt
092800030701     c                   endif
092900030701      *data scadenza
093000080930     c                   if        adtdst > 0
093100080930     c                   move      adtdst        dataiso
093200030701     c                   move      dataiso       dataeur
093300080930     c                   move      dataeur       vi2dst
093400030701     c                   else
093500080930     c                   clear                   vi2dst
093600030701     c                   endif
093700080930      *data scadenza
093800080930     c                   if        adtduv > 0
093900080930     c                   move      adtduv        dataiso
094000080930     c                   move      dataiso       dataeur
094100080930     c                   move      dataeur       vi2duv
094200080930     c                   else
094300080930     c                   clear                   vi2duv
094400080930     c                   endif
094500080930      *data scadenza
094600080930     c                   if        adtdcn > 0
094700080930     c                   move      adtdcn        dataiso
094800080930     c                   move      dataiso       dataeur
094900090324     c                   move      dataeur       vi2dcn
095000080930     c                   else
095100090324     c                   clear                   vi2dcn
095200080930     c                   endif
095300080930      *data scadenza
095400080930     c                   if        adtdts > 0
095500080930     c                   move      adtdts        dataiso
095600080930     c                   move      dataiso       dataeur
095700080930     c                   move      dataeur       vi2dts
095800080930     c                   else
095900080930     c                   clear                   vi2dts
096000080930     c                   endif
096100090402      *data scadenza
096200090402     c                   if        adtdrc > 0
096300090402     c                   move      adtdrc        dataiso
096400090402     c                   move      dataiso       dataeur
096500090402     c                   move      dataeur       vi2drc
096600090402     c                   else
096700090402     c                   clear                   vi2drc
096800090402     c                   endif
096900030701      *esco per raggiunta massima capacità sfl
097000030617     c                   if        snrr1 > 9990
097100030617     c                   leave
097200030617     c                   endif
097300030617
097400030604     C                   EVAL      snrr1 = snrr1 + 1
097500030604     C                   WRITE     video2
097600011228     C                   ENDDO
097700011228
097800030604     C                   IF        snrr1  > 0
097900020219     C                   EVAL      *IN91 = *ON
098000020219     C                   MOVE      1             OK                1 0
098100020219     C                   ELSE
098200020219     C                   MOVE      0             OK                1 0
098300030701     C                   ENDIF
098400030612     C                   EVAL      nrr1  = 1
098500011228     C                   ENDIF
098600020219     C                   EVAL      WrkCarS01 = *OFF
098700030604     C                   EVAL      snrr1  = 1
098800030604     C                   EVAL      csrrrn = 1
098900011228     C                   ENDSR
099000011227     c**********************************************************************
099100011227     c* gestione sfl
099200011227     c**********************************************************************
099300000613     C     GestioneSFL   BEGSR
099400000627      *
099500000627     C                   EVAL      WrkEofS01 = *OFF
099600000627      * F10 Immissione.
099700081013     C  n21              IF        *INKJ
099800080930     c                   clear                   video3
099801131021     c                   move      knmus         vknmus
099802131021     c                   move      knsif         vknsif
099803131021     c                   movel     ragut         rsut
099900081010     c                   seton                                        23
100000080930     c                   movel     vidfil        vadtpdr
100100080930     c                   move      vidpdr        vadtpdr
100200090522     c                   clear                   vadtpdrd
100300081001     c     kapd          chain     fiapd01l
100400090720     c                   if        %found(fiapd01l)
100500090507     c                   movel     apdrsf        vadtpdrd
100600081001     c                   endif
100700080930     c     vadtpdrk      setgt     fiadt01l
100800080930     c     vadtpdrk      readpe(n) fiadt01l
100900080930     c                   if        not %eof(fiadt01l)
101000090429     c                   if        adtdts = 0
101100090429     c                   eval      $msg = 'PROGRESSIVO PRECEDENTE NO-
101200090429     c                             N CONVALIDATO INSERIMENTO NON AMMESSO'
101300090429     c                   seton                                        2896
101400090429     C                   GOTO      NORIGA
101500090429     c                   endif
101600090612     c                   if        adtdcn = 0
101700090612     c                   exsr      srcdf
101800090612     c                   if        no_trs <> *blank
101900090612     c                   eval      $msg = 'Non trovato un contratto -
102000090612     c                             legato al fornitore inserimento  non ammesso'
102100090612     c                   seton                                        2896
102200090612     C                   GOTO      NORIGA
102300090612     c                   endif
102400090612     c                   endif
102500080930     c                   eval      vADTPRG =  ADTPRG + 1
102600081010     c                   move      adtdst        dataiso
102700081010     c                   move      dataiso       datacom
102800080930     c     datacom       adddur    1:*d          datacom
102900080930     c                   move      datacom       vadtddt
103000081009     c                   else
103100081009     c                   setoff                                       23
103200090114     c                   if        wuda > 20090131
103300081009     c                   z-add     wdat          vadtddt
103400081009     c                   movel     01            vadtddt
103500090114     c                   else
103600090114     c                   move      01022009      vadtddt
103700080930     c                   endif
103800090114     c                   endif
103900090114     c                   move      31122039      vadtdst
104000040728     c                   clear                   modalita
104100080930     c                   clear                   fiadt000
104200040728     c                   move      knmus         vknmus
104300040728     c                   move      knsif         vknsif
104400040728     c                   movel     ragut         rsut
104500040728     c                   exsr      Gesvideo3
104600081009     c                   seton                                        23
104700040728     c                   eval      wrkcars01 = *on
104800090429     c                   endif
104900040728     c                   if        csrrrn = 0
105000040728     c                   goto      noriga
105100040728     c                   endif
105200030605      * Elaborazione opzioni.
105300011228     c*
105400030605     c                   do        *hival        X                 4 0
105500030605     c     X             CHAIN     video2                             50
105600030605     C   50              LEAVE
105700030604     C                   IF        v1csce <> *blanks
105800030605     c                   exsr      opzione
105900030611
106000030611     c                   move      *blank        v1csce
106100030605     c                   update    video2
106200030611
106300011228     C                   ENDIF
106400030605     C                   enddo
106500040728      *
106600040728     c                   if        modalita = '1' or modalita = '3'
106700081007     c                             or modalita = '4' or modalita = '9'
106800090326     c                             or modalita = '2' or modalita = '7'
106900040728     c                   eval      wrkcars01 = *on
107000040728     c                   endif
107100000627      *
107200031003     C     noriga        ENDSR
107300090522      *--------------------------------------------------
107400090522     c     srcdf         begsr
107500090522      *--------------------------------------------------
107600090612     c                   clear                   no_trs            1
107700090522     c                   clear                   sogpartiva
107800090522     c                   clear                   sogdes
107900090522     c     kfrn          chain     anfrn01l
108000090522     c                   if        %found(anfrn01l)
108100090522     c     frnsogg       chain     ansog01l
108200090525     c                   if        %found(ansog01l)
108300090525     c                   movel     sogpartiva    ivak
108400090525     c     kaitrs2       chain     aitrs02l
108500090525     c                   if        %found(aitrs02l)
108600090525     c                   if        trsdec > adtddt
108700090525     c                   eval      $msg = 'Data decorrenza tariffa minore -
108800090525     c                             di data inizio contratto'
108900090525     c                   seton                                        2896
109000090525     c                   except    libera
109100090525     c                   endif
109200090612     c                   else
109300090612     c                   move      'X'           no_trs
109400090525     c                   endif
109500090525     c                   endif
109600090525     c                   endif
109700090522     c                   endsr
109800120220     C**************************************************************************
109900120220     C*    CONTROLLo per a/d se il fornitore è un trazionista certificato
110000120220     C**************************************************************************
110100120220     C     veriftraz     BEGSR
110200120510     c                   if        modalita = '2'
110300120510     C                   move      adtcsf        aAAcsf
110400120510     C                   move      *zeros        aAAcdf
110500120510     C                   move      adtcdf        aAAcdf
110600120510     c                   else
110700120510     C                   move      apdcsf        aAAcsf
110800120510     C                   move      *zeros        aAAcdf
110900120510     C                   move      apdksc        aAAcdf
111000120510     c                   endif
111100120220     c                   clear                   esito
111200120220     c                   clear                   sogpartiva
111300120321     c     kfrnAAA       chain     anfrn01l
111400120220     c                   if        %found(anfrn01l)
111500120220     c     frnsogg       chain     ansog01l
111600120220     c                   if        %found(ansog01l)
111700120518     c                   move      aaacsf        parsoc
111800120220     c                   movel     sogpartiva    pariva
111900120220     c                   call      'TRULTRAZ'
112000120220     c                   parm                    partraz
112100120306     c                   parm                    dsAIATS
112200120220     c                   endif
112300120220     c                   endif
112400120510      *se sono in modifica quindi data scadenza allegato e ho un errore di
112500120510      *congruenza con le date contratto al momento lo bypasso
112600120510     c                   if        modalita = '2' and
112700120717     c                             esito <> ' '
112800120605     c                   move      '1'           esito
112900120723     c                   endif
113000120220     c                   endsr
113100120723ab   C**************************************************************************
113200120723ab    * Controlla se nel periodo successivo è prevista un'altra società
113300120723ab   C**************************************************************************
113400120723ab   C     TEST_SOCieta  begSR
113500120723ab    *
113600120723ab    /FREE
113700120723ab
113800120723EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( adtpdrf
113900120723EDPDC                                             : dtVALIDITA
114000120723EDPDC                                             : 'O'
114100120723EDPDC                                             : valDatIniz
114200120723EDPDC                                             : valDatFine
114300120723ab                                                );
114400120723ab
114500120723EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
114600120723EDPDC    // Gestire l'errore.
114700120723ab     ENDIF;
114800120723ab
114900120723ab    /END-FREE
115000120724     c                   move      valdatfine    valdatfineW       8 0
115100120723ab   C                   ENDSR
115200011227     c**********************************************************************
115300000607     C     *INZSR        BEGSR
115400011227     c**********************************************************************
115500000607      *
115600000607     C     *ENTRY        PLIST
115700000607     C                   PARM                    Kpjba
115800030604     C                   Z-ADD     1             CODUT
115900030604     C                   CALL      'X§PARUT'
116000030604     C                   PARM                    UTEDSE
116100030604     C                   MOVEL     REC80         CNCR80
116200030703     C                   MOVEL     Ragut         rsut
116300030703     C                   MOVEL     knsif         vknsif
116400030703     C                   MOVEL     knmus         vknmus
116500030702      *reperimento data
116600030702     C                   TIME                    W0120            14 0
116700030702     C                   MOVE      W0120         WDAT              8 0
116800030702     C*
116900030702     C                   Z-ADD     WDAT          G02DAT
117000030702     C                   MOVEL     *BLANK        G02ERR
117100030702     C                   CALL      'XSRDA8'
117200030702     C                   PARM                    WLBDA8
117300030702     C* UDATE A 8 IN AAAA/MM/GG
117400030702     C                   Z-ADD     G02INV        WUDA              8 0
117500080930     C                   CLEAR                   Tibs36Ds
117600080930     C                   EVAL      I36ute = knmus
117700080930     C                   CALL      'TIBS36R'
117800080930     C                   PARM                    tibs36ds
117900030718
118000030718     C     Kapd          KLIST
118100030718     C                   KFLD                    tipoa             1
118200080930     C                   KFLD                    vadtpdrk          7 0
118300030718     C                   move      'D'           tipoa
118400090327     C     Kaitra        KLIST
118500090327     C                   KFLD                    vadtpdrk          7 0
118600090327     C                   KFLD                    data0             8 0
118700090417      *
118800090525     C     Kaitrs2       KLIST
118900090525     C                   KFLD                    adtcsf
119000090525     C                   KFLD                    ivak             16
119100090525     c                   kfld                    data0
119200090525      *
119300090417     C     Ktab          KLIST
119400090417     C                   KFLD                    tblkut
119500090417     C                   KFLD                    tblcod
119600090417     C                   KFLD                    tblkey
119700111124     C     Ktbe          KLIST
119800111124     C                   KFLD                    tbecod
119900111124     C                   KFLD                    tbeke1
120000111124     C                   KFLD                    tbeke2
120100000607      *
120200080930     C     Kadt          KLIST
120300080930     C                   KFLD                    vadtpdrk
120400081001     C                   KFLD                    vi2prg
120500090417      *
120600090522     C     Kfrn          KLIST
120700090522     C                   KFLD                    adtcsf
120800090522     C                   KFLD                    adtcdf
120900120321      *
121000120321     C     KfrnAAA       KLIST
121100120321     C                   KFLD                    aAAcsf
121200120321     C                   KFLD                    aAAcdf
121300090610      *
121400090610     C     Katt          KLIST
121500090610     C                   KFLD                    vidfil
121600090610     C                   KFLD                    vadtdstg
121700090522      *
123000111124     c* tabbella VPO record CONT
123100111124     C                   movel     'VPO'         tbecod
123200111124     C                   MOVEL(P)  'CONT'        tbeke1
123300111124     C                   clear                   tbeke2
123400111124     C                   clear                   dvpocont
123500111124     C     KTbe          CHAIN     Tntbe01l
123600111124     c                   if        %found(tntbe01l)
123700111124     C                   movel     tbeuni        dvpocont
123800111124     c                   else
123900111124     c                   clear                   dvpocont
124000111124     c                   end
124100090417     c* reperisce gg min durata allegato
124200090417     C                   movel     '5A'          tblcod
124300090417     C                   MOVEL(P)  'AUT'         tblKEY
124400090417     c                   z-add     1             tblkut
124500090508     c                   z-add     0             giomin
124600090417     C     KTAB          CHAIN     TABEL00F
124700090417     c                   if        %found(tabel00f)
124800090417     C                   movel     tbluni        ds5Aaut
124900090417     c                   if        §5AGGTARA <> 0
125000090417     C                   z-add     §5AGGTARA     giomin            3 0
125100090417     c                   end
125200090417     c                   end
125300120220     c                   move      '3I'          tblcod
125400120220     c                   movel(p)  '1'           tblkey
125500120220     c     ktab          chain     tabel00f
125600120220     c                   if        %found(tabel00f)
125700120220     c                   movel     tbluni        ds3i
125800120220     c                   eval      soctraini = %subst(§3IBST: 7: 3)
125900120220     c                   endif
126000090417      *
126100090417     C                   ENDSR
126200000607      *---------------------------------------------------------------------
126300090325     Ofiadt000  E            libera
