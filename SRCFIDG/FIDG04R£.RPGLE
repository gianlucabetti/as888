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
001900160920     Ffitgt01l  iF   E           k disk
002000011228      *
002100020103     D Psds           SDS
002200020103     D  PgmName          *PROC
002300011228     D Kpjba         E DS
002400011228      *
002500030721     D fnlv24ds      E DS
002600030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002700030604     D CNCR80        E DS
002800131003     D tibs42ds      E DS
002900131003     D uteautds      E DS
003000090114     d ficn05ds      e ds
003100090408     d ficn08ds      e ds
003200090114     d dgpi          e ds
003300120220     D ds3i          E DS
003400111124     D dvpocont      E DS
003500030617     d Tibs36Ds      e ds
003600090402     d dapdflr       e ds
003700090417     D DS5Aaut       E DS
003800120306     D DSaiats       E DS                  EXTNAME(AIATS00F)
003900011227     d data            ds            10
004000011228     d aa                      3      4
004100011228     d mm                      6      7
004200011228     d gg                      9     10
004300011228      *
004400030702     D fatto           S              1
004500120221     D soctraini       S              3
004600030610     D modalita        S              1
004700030604     D x               S              4  0
004800011227     D WrkEofS01       S              1
004900011227     D WrkCarS01       S              1
005000011228     D $VIDEO          S             10
005100030604     D snrr1           S              5i 0
005200120321      **
005300120321     d  aAAcsf         s                   like(aDTcsf)
005400120321     d  aAAcdf         s                   like(aDTcdf)
005500120321      **
005600030721     D Savkpjbu        S                   like(kpjbu)
005700080930     D SavOpzione      S                   like(v1csce)
005800090417     d isost           s               d   datfmt(*iso)
005900090417     d isodt           s               d   datfmt(*iso)
006000030606     d dataiso         s               d   datfmt(*iso)
006100030606     d dataeur         s               d   datfmt(*eur)
006200081010     d datacom         s               d   datfmt(*eur)
006300030613     D WLBDA8          DS
006400030613     D  G02DAT                 1      8  0
006500030613     D  G02INV                 9     16  0
006600030613     D  G02ERR                17     17
006700030613     D  G02TGI                18     22  0
006800080930     D param           ds
006900080930     D  parpdr                        7s 0
007000080930     D  parprg                        3s 0
007100080930     D  parmod                        1
007200120220     D partraz         DS
007300120518     D  parsoc                        3s 0
007400120502     D  pariva                       16
007500120502     D  pardtin                       8s 0
007600120502     D  pardtfi                       8s 0
007700120502     d  esito                         1
007800120502     d  errmsg                       75
007900030701      *---------------- ----------------------------------------------------
008000120723ab   **
008100120723ab   ** Prototipi.
008200120723ab    **
008300120723ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
008400120723ab    /DEFINE DFTACTGRP_NO
008500120723ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
008600120723ab    /UNDEFINE DFTACTGRP_NO
008700120723      *
008800120723ab   D TibsSof_esito...
008900120723ab   D                 S             10I 0 IMPORT
009000120723ab   D idSocieta...
009100120723EDPDCD                 S              3A
009200120723ab   **
009300120723EDPDCd dtVALIDITA      s               D
009400120723EDPDCd VALdatFINE      s               D
009500120723EDPDCd VALdatINIZ      s               D
009600030604      *---------------- Gestione formato video video01----------------------
009700120723ab   C*     Inizializza
009800120723ab    /FREE
009900120723ab     TibsSof_Init();
010000120723ab    /END-FREE
010100131014     c                   if        o36pou <> 046
010200131014     C                   EXSR      Inzvideo1
010300131014     c                   endif
010400030604     C                   DOU       $Video <> 'VIDEO1'
010500000608      *----------- Visualizzo dati di output in caso di errori.-------------
010600000608     C                   IF        *IN99
010700000608     C                   EVAL      *IN99 = *OFF
010800030604     C                   WRITE     video1
010900000608     C                   EVAL      *IN99 = *ON
011000000608     C                   ENDIF
011100000608      *
011200030702     c     emetti        tag
011300030604     C                   EXFMT     video1
011400000608      *
011500011227     C                   SELECT
011600030612     C                   WHEN      *INKC
011700011227     C                             OR
011800011227     C                             *INKL
011900011227     C                   EVAL      $Video = *BLANKS
012000011227
012100011227     C                   WHEN      *INKE
012200030604     C                   EXSR      Inzvideo1
012300011227     C                   WHEN      *INKB
012400030604     C                   EXSR      Chkvideo1
012500011227     C                   OTHER
012600030604     C                   EXSR      Chkvideo1
012700040728     c   84              goto      emetti
012800030604     C  N99              EXSR      Wrkvideo2c
012900011227     C                   ENDSL
013000011227      *
013100011227     C                   ENDDO
013200011227      *
013300011227     C                   EXSR      Uscita
013400011227     c**********************************************************************
013500011227     c* uscita
013600011227     c**********************************************************************
013700011227     C     Uscita        BEGSR
013800011227      *
013900011227     C                   EVAL      *INLR = *ON
014000011227     C                   RETURN
014100011227      *
014200011227     C                   ENDSR
014300011227     c**********************************************************************
014400011227     c* gestione sfl
014500011227     c**********************************************************************
014600030604     C     Wrkvideo2c    BEGSR
014700000627      *
014800011227     C                   EVAL      WrkCarS01 = *ON
014900030604     C                   EVAL      $Video = 'VIDEO2C'
015000011227      *
015100030604     C                   DOU       $Video <> 'VIDEO2C'
015200030604     C                   EXSR      Carvideo2
015300011228     c*Descrizione della ricerca
015400000627      *
015500030604     C                   WRITE     video2z
015600030604     C                   EXFMT     video2c
015700090429     c                   setoff                                       2896
015800090429     c                   clear                   $msg
015900011227     C                   SELECT
016000011228     c* fine
016100011227     C                   WHEN      *INKC=*ON
016200011227     C                   EVAL      $VIDEO=*BLANKS
016300011228     c* guida
016400011227     C                   WHEN      *INKL=*ON
016500030604     C                   EVAL      $VIDEO='VIDEO1'
016600011228     c* ripristino
016700011227     C                   WHEN      *INKE=*ON
016800011227     C                   EVAL      WrkCarS01 = *ON
016900011227
017000011227     C                   OTHER
017100011227
017200011227     C                   EXSR      GestioneSFL
017300011227     C                   ENDSL
017400011228
017500011227     C                   ENDDO
017600031027     C                   ENDSR
017700011227     c**********************************************************************
017800011227     c* Controlli video R01.
017900011227     c**********************************************************************
018000030604     C     Chkvideo1     BEGSR
018100000609      *
018200000608     C                   EVAL      *IN99 = *OFF
018300040728     C                   EVAL      *IN84 = *OFF
018400081009     C                   EVAL      *IN40 = *OFF
018500080930     c     vidfil        chain     azorg01l
018600080930     c                   movel     orgdes        vidfild
018700130808      * controllo autorizzazione del profilo ad accedere alle tariffe AFF/DEF
018800131003     c                   clear                   tibs42ds
018900131003     c                   movel     vidfil        i42pge
019000131003     c                   call      'TIBS42R'
019100131003     c                   parm                    tibs42ds
019200131003     c                   movel     o42uni        uteautds
019300130808     c                   if        §affmtc <> 'S'
019400130808     c                   exsr      uscita
019500130808     c                   endif
019600081013      *modalità interrogazione proteggo i campi e passo ai chiamati opzione interrogazione
019700081013     c                   if        kcdaz = 'FNA4'
019800081013     c                   seton                                        2125
019900081013     c                   move      'I'           parmod
020000081013     c                   endif
020100080930      *
020200080930     C****  CODICE PADRONCINO  ****
020300080930     C     '?'           SCAN      vidpdr                                 55
020400080930     C*
020500080930     C     *IN55         IFEQ      *ON
020600080930     C                   MOVEL     KPJBU         SAVKPJBU
020700080930     C                   Z-ADD     Vidfil        d24FIL
020800080930     C                   Z-ADD     0             d24pdr
020900080930     C                   MOVEL     'R'           d24FLG
021000080930     C                   MOVEL     'D'           d24tip
021100080930     C                   MOVEL     *BLANKS       d24rsc
021200080930     C                   MOVEL(p)  fnlv24ds      KPJBU
021300080930     C                   CALL      'FNLV24R'
021400080930     C                   PARM                    KPJBA
021500080930     C                   MOVEL     KPJBU         fnlv24ds
021600080930     C                   MOVEL     savKPJBU      KPJBU
021700080930     C*
021800080930     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
021900080930     C     d24pdr        IFNE      0
022000080930     C                   MOVEL     d24pdr        Vidfil
022100080930     C                   MOVE      d24pdr        Vidpdr
022200080930     C                   MOVEL     d24rsc        despdr
022300080930     c                   else
022400080930     C                   clear                   Vidpdr
022500080930     C                   clear                   Vadtpdrk
022600080930     C                   clear                   despdr
022700081009     c                   seton                                        4084
022800080930     C                   ENDIF
022900080930     C*
023000080930     C                   ENDIF
023100080930      * decodifica autista
023200081009     c                   if        vidfil = 0 or vidpdr = *zeros
023300081009     c                             or vidpdr = *blank
023400081009     c                   seton                                        4084
023500081009     c                   endif
023600080930     c                   if        vidfil  <> *zeros and vidpdr <> *blank
023700080930     c                             and vidpdr > '0000'
023800080930     c                   movel     vidfil        vadtpdrk
023900080930     c                   move      vidpdr        vadtpdrk
024000081001     c                   clear                   despdr
024100081001     c     kapd          chain     fiapd01l
024200090720     c                   if        %found(fiapd01l)
024300090720     c                   if        apdatb = *blank or
024400090720     c                             apdatb <>*blank and parmod ='I'
024500090507     c                   movel     apdrsf        despdr
024600081001     c                   else
024700081009     c                   seton                                        4084
024800081001     c                   endif
024900091109     c                   else
025000091109     c                   seton                                        4084
025100090720     c                   endif
025200080930     c                   endif
025300000608      *
025400000608     C                   ENDSR
025500011227     c**********************************************************************
025600011227     c* inizializza r01
025700011227     c**********************************************************************
025800030604     C     Inzvideo1     BEGSR
025900000608
026000080930     c                   move      o36pou        vidfil
026100030604     C                   EXSR      Chkvideo1
026200000609
026300000608     C                   ENDSR
026400030604     c**********************************************************************
026500030605     C     opzione       BEGSR
026600030605     c**********************************************************************
026700030610     c                   move      v1csce        modalita
026800040728     c                   movea     '000000000000'*in(40)
026900040728     c                   movea     '00000'       *in(52)
027000081010     c* se non è ultimo progressivo proteggo la data scadenza in quanto
027100081010     c* si andrebbe a creare un buco temporale fra data scadenza e data decorrenza
027200081010     c* del progressivo precedente nel caso in cui venisse modificata
027300081010     c                   seton                                        23
027400081010     c                   setoff                                       22
027500081010     c                   if        vi2prg < lastprg
027600081010     c                   seton                                        22
027700081010     c                   endif
027800040728     c                   setoff                                       96
027900030605     c                   select
028000080930     c                   when      v1csce = 'D'
028100080930     c                   exsr      dettagli
028200081013     c                   when      v1csce = '2' and not *in21
028300030605     c                   exsr      modifica
028400090608     c                   when      v1csce = '3' and not *in21  or
028500090608     c                             v1csce = '9' and not *in21
028600090508     c                   exsr      copia
028700090324     c                   when      v1csce = '7'
028800090324     c                             and not *in21
028900090324     c                   exsr      modifica
029000090324     c                   when      v1csce = '8'
029100090324     c                             and not *in21
029200090324     c                   exsr      stampa
029300090324     c                   exsr      modifica
029400030605     c                   when      v1csce = '5'
029500030606     c                   seton                                        21
029600030606     c                   exsr      modifica
029700081013      *non spengo indicatore se sono in interrogazione
029800081013     c                   if        kcdaz <> 'FNA4'
029900030606     c                   setoff                                       21
030000081013     c                   endif
030100081013     c                   when      v1csce = '4' and not *in21
030200090609     c*                  seton                                        21
030300040728     c                   exsr      modifica
030400040728     c                   setoff                                       2128
030500040728     c                   clear                   $msg
030600030605     c                   endsl
030700030604     C                   ENDSR
030800030605     c**********************************************************************
030900030605     C     modifica      BEGSR
031000030605     c**********************************************************************
031100090522     c                   if        modalita = '5'
031200090522     c     kadt          chain(n)  fiadt01l                           98
031300090522     c                   else
031400090522     c     kadt          chain     fiadt01l                           98
031500090522     c                   endif
031600030610     c                   if        not *in98
031700080930     c                   move      ADTPDR        vADTPDR
031800080930     c                   move      ADTPRG        vADTPRG
031900080930     c                   move      ADTDDT        vADTDDT
032000080930     c                   move      ADTDST        vADTDST
032100080930     c                   move      ADTDCN        vADTDCN
032200080930     c                   move      ADTDTS        vADTDTS
032300080930     c                   move      ADTDUV        vADTDUV
032400111205     c                   z-add     ADTpcar       vADTPcar
032500120104     c     adttsr        comp      'R'                                    20
032600111214     c                   seton                                        88
032700090402     c                   z-add     adtnrc        vadtnrc
032800090402     c                   z-add     adtfil        vadtfil
032900090402     c                   z-add     adtpkl        vadtpkl
033000090408     c                   z-add     adttmpc       vadttmpc
033100090408     c                   z-add     adttmps       vadttmps
033200090428     c                   move      adtcdf        vADTcdf
033300090428     c                   move      adtcsf        vADTcsf
033400090522     c                   clear                   vdesforn
033500090522     c     kfrn          chain     anfrn01l
033600090522     c                   if        %found(anfrn01l)
033700090522     c                   movel     frndesbrev    vdesforn
033800090522     c                   endif
033900110928     c                   if        modalita ='4' and vadtdts > 0
034000110928     c                   if        %subst(knmus:1:3) <> 'EDP'
034100081009     c                   eval      $msg = 'Tariffa non cancellabile -
034200090324     c                             già convalidata'
034300090609     c                   seton                                        289621
034400090609     c                   except    libera
034500110928     c                   else
034600110928      * controllo che la data scadenza  non sia inferiore ad eventuali valorizzazioni
034700110928     c     katt          setgt     fiatt01l
034800110928     c                   do        *hival
034900110928     c     vidfil        reade     fiatt01l
035000110928     c                   if        %eof(fiatt01l)
035100110928     c                   leave
035200110928     c                   endif
035300110928     c                   if        attpdr = vadtpdr  and
035400110928     c                             attddc >= adtddt and attddc <= adtdst
035500110928     c                   eval      $msg = 'Tariffa non cancellabile -
035600110928     c                             esistono valorizzazioni per l''autista'
035700110928     c                   seton                                        289621
035800110928     c                   except    libera
035900110928     c                   endif
036000110928     c                   enddo
036100110928     c*
036200110928     c                   endif
036300081009     c                   endif
036400090324     c                   if        modalita ='7' and vadtdts > 0
036500090324     c                   eval      $msg = 'Tariffa -
036600090324     c                             già convalidata'
036700090324     c                   seton                                        2896
036800160920     c                   except    libera
036900090525     c                   endif
037000090324     c                   if        modalita ='8'
037100090324     c                   eval      $msg = 'Tariffa stampata'
037200090609     c                   seton                                        289621
037300090325     c                   except    libera
037400090324     c                   endif
037500030718      * decodifica autista
037600081001     c                   clear                   vadtpdrd
037700081001     c     kapd          chain     fiapd01l
037800090720     c                   if        %found(fiapd01l)
037900090507     c                   movel     apdrsf        vadtpdrd
038000081001     c                   endif
038100090408      * se non presente imposta peso da anagrafica gestionale
038200090417     c                   if        adttmpc = 0 or adttmps = 0
038300090417     c                   clear                   ficn08ds
038400090417     c                   move      'D'           i08tip
038500090417     c                   z-add     adtddt        i08data
038600090417     c                   z-add     vadtpkl       i08mcp
038700090417     c                   call      'FICN08R'
038800090417     C                   parm                    ficn08ds
038900090417     c                   if        o08err = *blank
039000090417     c                   z-add     o08tc         vadttmpc
039100090417     c                   z-add     o08ts         vadttmps
039200090417     c                   else
039300090408     c                   clear                   vadttmpc
039400090408     c                   clear                   vadttmps
039500090408     c                   endif
039600090408     c                   endif
039700030701      *data decorrenza
039800080930     c                   if        adtddt > 0
039900080930     c                   move      adtddt        dataiso
040000030701     c                   move      dataiso       dataeur
040100080930     c                   move      dataeur       vadtddt
040200030701     c                   else
040300080930     c                   clear                   vadtddt
040400030701     c                   endif
040500030701      *data scadenza
040600080930     c                   if        adtdst > 0
040700080930     c                   move      adtdst        dataiso
040800030606     c                   move      dataiso       dataeur
040900080930     c                   move      dataeur       vadtdst
041000030630     c                   else
041100080930     c                   clear                   vadtdst
041200030606     c                   endif
041300080930     c                   if        adtdcn > 0
041400080930     c                   move      adtdcn        dataiso
041500080930     c                   move      dataiso       dataeur
041600080930     c                   move      dataeur       vadtdcn
041700080930     c                   else
041800080930     c                   clear                   vadtdcn
041900080930     c                   endif
042000120104     c* ristampa
042100120104     c* se è una ristampa si deve riempire la "data di stampa" con
042200120104     c* la 1° data certa, e mettere nella "data di ristampa"
042300120104     c* l'ultima data certa ...
042400120104     c                   if        adttsr = 'R'
042500120104     c                   if        adtunodc > 0
042600120104     c                   move      adtunodc      dataiso
042700120104     c                   move      dataiso       dataeur
042800120104     c                   move      dataeur       vadtunodc
042900120104     c                   else
043000120104     c                   clear                   vadtunodc
043100120104     c                   endif
043200120104     c                   eval      vadtunodc = vadtdcn
043300120104     c                   if        adtunodc <> 0
043400120104     c                   move      dataeur       vadtdcn
043500120104     c                   else
043600120104     c* tariffe prima di dicembre 2011 quando non esisteva ancora
043700120104     c* il campo della prima data certa
043800120104     c                   clear                   vadtdcn
043900120104     c                   end
044000120104     c                   end
044100120104     c*
044200080930     c                   if        adtdts > 0
044300080930     c                   move      adtdts        dataiso
044400080930     c                   move      dataiso       dataeur
044500080930     c                   move      dataeur       vadtdts
044600080930     c                   else
044700080930     c                   clear                   vadtdts
044800080930     c                   endif
044900090324     c                   if        adtdrc > 0
045000090324     c                   move      adtdrc        dataiso
045100090324     c                   move      dataiso       dataeur
045200090324     c                   move      dataeur       vadtdrc
045300090324     c                   else
045400090324     c                   clear                   vadtdrc
045500090324     c                   endif
045600080930     c                   if        adtduv > 0
045700080930     c                   move      adtduv        dataiso
045800080930     c                   move      dataiso       dataeur
045900080930     c                   move      dataeur       vadtduv
046000080930     c                   else
046100080930     c                   clear                   vadtduv
046200080930     c                   endif
046300030605     c                   exsr      Gesvideo3
046400030605     c                   endif
046500030605     C                   ENDSR
046600030605     c**********************************************************************
046700030605     C     copia         BEGSR
046800030605     c**********************************************************************
046900090508     c                   clear                   video3
047000131021     c                   move      knmus         vknmus
047100131021     c                   move      knsif         vknsif
047200131021     c                   movel     ragut         rsut
047300090508     c                   seton                                        23
047400090508     c     Vadtpdrk      setgt     fiadt01l
047500090508     c     Vadtpdrk      readpe(n) fiadt01l
047600090508     c                   if        adtdts = 0
047700090508     c                   eval      $msg = 'PROGRESSIVO PRECEDENTE NO-
047800090508     c                             N CONVALIDATO INSERIMENTO NON AMMESSO'
047900090508     c                   seton                                        2896
048000090508     C                   leavesr
048100090508     c                   endif
048200090508     c                   move      ADTPDR        vADTPDR
048300090508     c                   eval      vADTPRG =  ADTPRG + 1
048400090508     c                   move      adtdst        dataiso
048500090508     c                   move      dataiso       datacom
048600090508     c     datacom       adddur    1:*d          datacom
048700090508     c                   move      datacom       vadtddt
048800080930      * decodifica autista
048900090508     c                   clear                   vadtpdrd
049000090508     c     kapd          chain     fiapd01l
049100090720     c                   if        %found(fiapd01l)
049200090508     c                   movel     apdrsf        vadtpdrd
049300090508     c                   endif
049400090508     c                   clear                   fiadt000
049500090525     c                   clear                   vADTDST
049600090525     c                   clear                   vADTDCN
049700120104     c                   clear                   vADTunodc
049800090525     c                   clear                   vADTDTS
049900090525     c                   clear                   vADTDUV
050000090508     c                   exsr      Gesvideo3
050100030605     C                   ENDSR
050200090324     c**********************************************************************
050300090324     C     stampa        BEGSR
050400090324     c**********************************************************************
050500090324     c                   clear                   param
050600090324     c                   z-add     vadtpdrk      parpdr
050700090324     c                   z-add     vi2prg        parprg
050800090324     c                   movel     param         kpjbu
050900090324     c                   call      'FICNA6R1'
051000090324     c                   parm                    kpjba
051100090324     c
051200090324     C                   ENDSR
051300081007     c**********************************************************************
051400081007     C     copiadettaglioBEGSR
051500081007     c**********************************************************************
051600081007     c                   clear                   video3
051700131021     c                   move      knmus         vknmus
051800131021     c                   move      knsif         vknsif
051900131021     c                   movel     ragut         rsut
052000081007     c     kadt          setll     fiadd01l
052100081007     c                   do        *hival
052200081007     c     kadt          reade     fiadd01l
052300081007     c                   if        %eof(fiadd01l)
052400081007     c                   leave
052500081007     c                   endif
052600081007     c                   if        addatb <> *blank
052700081007     c                   iter
052800081007     c                   endif
052900081007
053000081007     c                   z-add     ADTPDR        ADdPDR
053100081007     c                   z-add     ADTPRG        addprg
053200081007     c                   move      ADDTSR        addTSR
053300081007     c                   z-add     ADDLNP        addLNP
053400081007     c                   z-add     ADDLNA        addLNA
053500081007     c                   z-add     ADDIMP        addIMP
053600081007     c                   move      ADDDIV        addDIV
053700081007     c                   move      ADDNOTE       addNOTE
053800081007     c                   z-add     ADDKM         addKM
053900081007     c                   write     fiadd000
054000081007     c                   enddo
054100081007     C                   ENDSR
054200090703     c**********************************************************************
054300090703     C     annuldettaglioBEGSR
054400090703     c**********************************************************************
054500090703     c                   clear                   video3
054600131021     c                   move      knmus         vknmus
054700131021     c                   move      knsif         vknsif
054800131021     c                   movel     ragut         rsut
054900090703     c     kadt          setll     fiadd01l
055000090703     c                   do        *hival
055100090703     c     kadt          reade     fiadd01l
055200090703     c                   if        %eof(fiadd01l)
055300090703     c                   leave
055400090703     c                   endif
055500090703     c                   move      'A'           addatb
055600090703     c                   update    fiadd000
055700090703     c                   enddo
055800090703     C                   ENDSR
055900080930     c**********************************************************************
056000080930     C     dettagli      BEGSR
056100080930     c**********************************************************************
056200080930     c                   clear                   param
056300080930
056400081013     c                   if        kcdaz  = 'FNA4'
056500081013     c                   move      'I'           parmod
056600081013     c                   else
056700080930     c                   clear                   parmod
056800080930     c                   if        vi2dts > 0
056900080930     c                   move      '5'           parmod
057000080930     C                   endif
057100081013     C                   endif
057200080930     c                   z-add     vadtpdrk      parpdr
057300080930     c                   z-add     vi2prg        parprg
057400080930     c                   movel     param         kpjbu
057500080930     c                   call      'FICNA4R'
057600080930     C                   PARM                    KPJBA
057700080930     C
057800080930     c                   endsr
057900030605     c**********************************************************************
058000030605     C     gesvideo3     BEGSR
058100030605     c**********************************************************************
058200030702     c                   clear                   fatto
058300120307     c                   clear                   forzato           1
058400120307     c                   setoff                                       29
058500090324     c                   do        *hival
058600030605     c                   exfmt     video3
058700081001     c                   setoff                                       2896
058800120307      *
058900120307      *** Forza con il comando F11 il controllo di inesistenza
059000120307      *    dell'autista tra i contratti dei Trazionisti.
059100120307     c   29              if        *inKk
059200120307     c                   eval       forzato = 'S'
059300120307     c                   setoff                                       29
059400120307     c                   end
059500120307     c
059600081001     c                   clear                   $msg
059700030605     c   kl              leave
059800040728     c  n21              exsr      controv3
059900030624     c   96              iter
060000090609     c                   if        not *in21
060100040728     c   kf              exsr      aggiorna
060200090609     c                   endif
060300030702     c                   if        fatto <> *blank
060400030702     c                   leave
060500030702     c                   endif
060600030605
060700030605     c                   enddo
060800030605     C                   ENDSR
060900030605     c**********************************************************************
061000030624     C     Controv3      BEGSR
061100030605     c**********************************************************************
061200030613      *
061300030702      * *in96 errore generico videata dettaglio
061400040728     c                   setoff                                       9628
061500040728     c                   clear                   $msg
061600040728     c                   if        modalita ='4'
061700040728     c                   goto      endctr
061800040728     c                   endif
061900120502      *controlli data  decorrenza
062000120502     c                   setoff                                       505253
062100120502     c                   if        vadtddt > 0
062200120502     C                   MOVE      Vadtddt       G02DAT
062300120502     C                   MOVEL     *BLANK        G02ERR
062400120502     C                   CALL      'XSRDA8'
062500120502     C                   parm                    wlbda8
062600120502     C     G02ERR        IFEQ      '1'
062700120502     C                   SETON                                        5096
062800120502     c                   leavesr
062900120502     C                   END
063000120502     c                   move      g02dat        vadtddt
063100120502     c                   move      g02inv        vadtddtg          8 0
063200120502     c                   endif
063300120502      *data scadenza obbligatoria
063400120502     c                   setoff                                       54
063500120502     c                   if        vadtdst =  *zeros
063600120502     c                   seton                                        5496
063700120502     c                   leavesr
063800120502     c                   endif
063900120502      *controlli data  scadenza
064000120502     c                   setoff                                       51
064100120502     c                   if        vadtdst > 0
064200120502     C                   MOVE      Vadtdst       G02DAT
064300120502     C                   MOVEL     *BLANK        G02ERR
064400120502     C                   CALL      'XSRDA8'
064500120502     C                   PARM                    WLBDA8
064600120502     C     G02ERR        IFEQ      '1'
064700120502     C                   SETON                                        5196
064800120502     c                   leavesr
064900120502     C                   END
065000120502     c                   move      g02dat        vadtdst
065100120502     c                   move      g02inv        vadtdstg          8 0
065200120502     c                   endif
065300120308     c                   clear                   flg_TRAZattiv     1
065400120308      **
065500120308      **   per prima cosa deve sapere se si tratta di un TRAZIONISTA ATTIVO
065600120308      ***  oppure NO
065700120308      *               *-----------------------*
065800120502     c                   clear                   partraz
065900120502     c                   z-add     vadtddtg      pardtin
066000120502     c                   z-add     vadtdstg      pardtfi
066100120308     c                   exsr      veriftraz
066200120308      *               *-----------------------*
066300120717     c                   if        esito <> ' '
066400120308     c                   eval      flg_TRAZattiv='S'
066500120308      **
066600120308      ***  Se TRAZIONISTA Accreditato:
066700120308      ***  La società immessa non coincide con quella dei TRAZIONISTI
066800120308      ***   ma.....
066900120308      ***    Questo può essere Forzabile per poter proseguire.
067000120308     c                   if        apdcsf <> *blank and apdcsf <> soctraini
067100120308      *
067200120308      * se forza con F11 NON deve + emettere questo controllo
067300120605     c                   if         forzato <> *blank
067400120308     c                   eval      $msg = 'Se Fornitore fa TRAINI STANDARD,-
067500120308     c                              la società deve essere '
067600120308     c                              + soctraini + '. F11 x SOC.diversa'
067700120308     c                   seton                                        2896
067800120308     c                   seton                                        29
067900120308     c                   leavesr
068000120308     c                   end
068100120308      ***
068200120308     c                   endif
068300120502      *** gestione errore in verifica trazionista
068400120717     c*m                 else
068500120717     c*m                 if        esito = '1'
068600120717     c*m                 eval      $msg = errmsg
068700120717     c*m                 seton                                        2896
068800120717     c*m                 leavesr
068900120717     c*m                 endif
069000120308     c                   endif
069100120724      *verifica congruenza data scadenza tariffa con società operativa corretta
069200130110     c                   if        modalita ='2' and vadtdst > 0 and esito = ' '
069300121219     c                             and adtcsf <> ' '
069400130110     c********************         vadtdst <> 31122039
069500130110     c**  Tolto il diverso da infinito perchè avendo spostato il controllo della
069600130110     c**  società basato sulla data scadenza e non decorrenza NON deve esserci più,
069700130110     c**  altrimenti, ciò permetterebbe di riaprire una tariffa fatta scadere sulla
069800130110     c**  precedente società, riattivandola con scadenza 31122039 e il controllo
069900130110     c**  verrebbe saltato. Invece deve essere controllata la Società Operativa!!
070000120724     c                   movel     adtpdr        adtpdrf           3 0
070100121219     c                   move      vadtdstg      dtvalidita
070200120724ab   C                   exsr      TEST_societa
070300121219     c                   if        idsocieta <> adtCSF
070400121218      *
070500120724ab   c                   seton                                        2896
070600120724     c                   eval      $msg = 'Attenzione data scadenza -
070700121219     c                             incongruente alla data validità -
070800120724     c                             della società operativa'
070900120724     c                   leavesr
071000120724ab   C                   end
071100120724ab   C                   end
071200120306     ***
071300120724     ***  Solo se in convalida della tariffa:
071400160920 1   c                   if        modalita ='7' and adtdts = 0
071500120723      *test società
071600160920 2   c                   if        esito = ' '
071700120723     c                   movel     adtpdr        adtpdrf           3 0
071800121219     c                   move      adtddt        dtvalidita
071900160920     C                   exsr      TEST_societa
072000160920 3   c                   if        apdcsf <> idsocieta
072100160920     c                   seton                                        2896
072200120723     c                   eval      $msg = 'Tariffa non Convalidabile -
072300120723     c                             società fornitore non congruente -
072400120723     c                             alla società operativa'
072500120723     c                   leavesr
072600160920e3   C                   end
072700120724     c                   move      adtdst        dataiso
072800160920 3   c                   if        adtdst > valdatfinew
072900160920     c                   seton                                        2896
073000120724     c                   eval      $msg = 'Scadenza tariffa maggiore -
073100120724     c                             di data scadenza della società -
073200120724     c                             operativa'
073300120724     c                   leavesr
073400160920e3   C                   end
073500160920e2   C                   end
073600120723      *
073700090525     c                   clear                   sidet
073800090525     c     kadt          setll     fiadd01l
073900090525     c                   do        *hival
074000090525     c     kadt          reade(n)  fiadd01l
074100090525     c                   if        %eof(fiadd01l)
074200090525     c                   leave
074300090525     c                   endif
074400090525     c                   if        addatb = *blank
074500090525     c                   move      'X'           sidet             1
074600090525     c                   leave
074700090525     c                   endif
074800090525     c                   enddo
074900090525     c                   if        sidet = *blank
075000090525     c                   eval      $msg = 'Tariffa non Convalidabile -
075100090525     c                             manca dettaglio'
075200090525     c                   seton                                        2896
075300090525     c                   leavesr
075400090525     c                   endif
075500120220      *controllo se trazionista il fornitore deve essere in società TRAINI
075600120321     c                   if        apdcsf <> *blank and apdcsf <> soctraini
075700120307      ***
075800120308      ***+++XX sono state portate fuori dalla IF che lo eseguiva solo in Conferma Tariffa
075900120321     c                   move      apdcsf        adtcsf
076000120321     C                   move      *zeros        adtcdf
076100120321     C                   move      apdksc        adtcdf
076200120321     c                   endif
076300160920      * test provvisorio verifica che non esista tariffa con stessa chiave su autista di città
076400160920      * da rimuovere eventualmente una volta che su docflow saranno sbinate le cartelle città e aff.
076500160920     c     ktgt          setll     fitgt01l
076600160920     c                   do        *hival
076700160920     c     ktgt          reade     fitgt01l
076800160920     c                   if        %eof(fitgt01l)
076900160920     c                   leave
077000160920     c                   endif
077100160920     c                   if        adtddt = tgtddt and
077200160920     c                             adtcdf = tgtcdf and
077300160920     c                             adtcsf = tgtsoc
077400160920     c                   eval      $msg = 'Tariffa con riferimenti uguali ad -
077500160920     c                             autista di città avvisare il CED x gestirla'
077600160920     c                   seton                                        2896
077700160920     c                   leavesr
077800160920     c                   endif
077900160920     c                   enddo
078000160920      * fine test provvisorio
078100120307      ***
078200160920e1   c                   endif
078300120308      *** ---------------------------------------------
078400090327      * controllo che la data inserimento non sia inferiore al 01/05/2009
078500090325      * data partenza procedura
078600090325     c                   if        vadtddtg < 20090501
078700090114     c                   seton                                        5096
078800090417     c                   leavesr
078900090114     c                   endif
079000111124     c* le supertestate devono essere inserite solo se non presente
079100111124     c* la data di blocco impostata nella tabella VPO record CONT
079200111124     c                   if        vadtddtg >= §vpodpa
079300111124     C                   SETON                                        5096
079400111124     C                   leavesr
079500111124     c                   end
079600120308      ** ------------------------------------------------
079700090327      * verifico se esiste un contratto e se esiste la data decorrenza non può essere
079800090327      * inferiore alla data decorrenza del contratto
079900120308      ***
080000120308     c                   if        flg_TRAZattiv <>'S' or forzato ='S'
080100120308      ***
080200090327     c     kaitra        chain     aitra05l
080300090327     c                   if        %found(aitra05l)
080400090611     c                   if        tradin > vadtddtg
080500090611     c                   seton                                        5396
080600090611     c                   leavesr
080700090611     c                   endif
080800090327     c     tranrc        chain     aitrs01l
080900090327     c                   if        %found(aitrs01l)
081000090327     c                   if        trsdec > vadtddtg
081100090611     c                   seton                                        5296
081200090417     c                   leavesr
081300090327     c                   endif
081400090327     c                   endif
081500090525     c                   else
081600090525      * se non è accreditato provo con il fornitore se c'è
081700090525     c                   if        adtcdf <> *blank
081800090525      *prima di impostare data convalida verifica se esiste contratto congruente
081900090525     c                   exsr      srcdf
082000090525     c                   endif
082100090327     c                   endif
082200120308      *---
082300120308     c                   endif
082400120308      *------------
082500090114      * controllo che la data scadenza  non sia maggiore del 31/12/2039
082600090114     c                   if        vadtdstg > 20391231
082700090114     c                   seton                                        5196
082800090417     c                   leavesr
082900090114     c                   endif
083000120306      ***
083100090610      * controllo che la data scadenza  non sia inferiore ad eventuali valorizzazioni
083200090610     c                   setoff                                       57
083300090610     c                   if        vadtdstg > 0 and vadtdstg <> 20391231
083400090610     c     katt          setgt     fiatt01l
083500090610     c                   do        *hival
083600090610     c     vidfil        reade     fiatt01l
083700090610     c                   if        %eof(fiatt01l)
083800090610     c                   leave
083900090610     c                   endif
084000090610     c                   if        attpdr = vadtpdr
084100090610     c                   seton                                        5796
084200090610     c                   leavesr
084300090610     c                   endif
084400090610     c                   enddo
084500090610     c                   endif
084600090417      * controllo che la data scadenza  non sia inferiore al numero di giorni imposti
084700090417      * per la durata minima della tariffa
084800090417     c                   setoff                                       56
084900090417     c                   if        vadtdstg > 0 and vadtddtg > 0
085000090417     c                   move      vadtdstg      isost
085100090417     c                   move      vadtddtg      isodt
085200090417     c     isost         subdur    isodt         ggvalidi:*d       4 0
085300090417     c                   if        ggvalidi < giomin
085400090417     c                   seton                                        5696
085500090417     c                   leavesr
085600090417     c                   endif
085700090417     c                   endif
085800030702      *controllo data decorrenza maggiore data scadenza
085900030702     c                   setoff                                       47
086000080930     c                   if        vadtddtg > vadtdstg
086100030702     c                   seton                                        4796
086200090417     c                   leavesr
086300030702     c                   endif
086400090114     c                   if        vadtpcar = 0
086500090114     c                   clear                   ficn05ds
086600090114     c                   call      'FICN05R'
086700090114     c                   parm                    ficn05ds
086800090114     c                   eval      dgpi = o05gpi
086900090114     c                   z-add     D§GPIGPI      vadtpcar
087000120104     c     adttsr        comp      'R'                                    20
087100111214     c                   seton                                        88
087200090114     c                   endif
087300040728     C     endctr        ENDSR
087400030605     c**********************************************************************
087500030605     C     aggiorna      BEGSR
087600030605     c**********************************************************************
087700080930     c                   move      vADTPDR       ADTPDR
087800080930     c                   move      vADTPRG       ADTPRG
087900111205     c                   z-add     vADTpcar      ADTPcar
088000090402     c                   z-add     vADTpkl       ADTPkl
088100090408     c                   z-add     vADTtmpc      ADTtmpc
088200090408     c                   z-add     vADTtmps      ADTtmps
088300080930     c                   if        vadtddt > 0
088400080930     c                   move      vadtddt       dataeur
088500030610     c                   move      dataeur       dataiso
088600080930     c                   move      dataiso       adtddt
088700030610     c                   endif
088800080930     c                   if        vadtdst > 0
088900080930     c                   move      vadtdst       dataeur
089000030610     c                   move      dataeur       dataiso
089100080930     c                   move      dataiso       adtdst
089200030610     c                   endif
089300081002     c                   z-add     wuda          adtduv
089400090324      * convalida tariffa
089500090324     c                   if        modalita = '7' and adtdts = 0
089600090324     c                   z-add     wuda          adtdts
089700090327      * imposta fornitore da anagrafica gestionale
089800090427     c                   move      *zeros        adtcdf
089900090427     c                   move      apdksc        adtcdf
090000090327     c                   eval      adtcsf = apdcsf
090100120220      * se al momento della convalida verifico che il fornitore è un trazionista
090200120220      * forzo tutti 9 nel contratto e nella filiale gestione per riconoscere che non
090300120220      * necessita di accreditamento da parte della automezzi e può essere stampato
090400120502     c                   clear                   partraz
090500120502      * inseriti anche qui i parametri per controllo delle date per ora asteriscati
090600120502      * da verificare se qui sono necessari essendo il controllo completo attivo nella
090700120502      * routine di controllo qui può bastare sapere solo se è o non è trazionista
090800120518     c                   z-add     adtddt        pardtin
090900120518     c                   z-add     adtddt        pardtin
091000120220     c                   exsr      veriftraz
091100120220     c                   endif
091200090324      * inserimento e copia scrive altrimenti aggiorna il record esistente
091300040728     c                   select
091400040728     c                   when      modalita = '4'
091500081009      *elimina dettaglio
091600081009     c     kadt          setll     fiadd01l
091700081009     c                   do        *hival
091800081009     c     kadt          reade     fiadd01l
091900081009     c                   if        %eof(fiadd01l)
092000081009     c                   leave
092100081009     c                   endif
092200081009     c                   delete    fiadd000
092300081009     c                   enddo
092400080930     c                   delete    fiadt000
092500081009
092600040728     c                   when      modalita = *blank or modalita = '3'
092700081007     c                             or modalita = '9'
092800080930     c                   write     fiadt000
092900081007      *
093000081007     c                   if        modalita = '9'
093100081007     c                   exsr      copiadettaglio
093200081007     c                   endif
093300090324      *
093400040728     c                   other
093500080930     c                   update    fiadt000
093600040728     c                   endsl
093700030702     c                   move      'X'           fatto
093800030605     C                   ENDSR
093900011228     c**********************************************************************
094000011228     c* carica sfl
094100011228     c**********************************************************************
094200030604     C     Carvideo2     BEGSR
094300011228     C                   IF        WrkCarS01 = *ON
094400030604     C                   EVAL      snrr1   = 0
094500011228     C                   EVAL      *IN90 = *ON
094600020219     C                   EVAL      *IN91 = *OFF
094700030604     C                   WRITE     video2c
094800011228     C                   EVAL      *IN90 = *OFF
094900080930     c                   movel     vidfil        vadtpdrk
095000080930     c                   move      vidpdr        vadtpdrk
095100080930     c     vadtpdrk      setll     fiadt01l
095200030701     c                   do        *hival
095300080930     c     vadtpdrk      reade     fiadt01l                               97
095400030701     c   97              leave
095500080930     c                   if        adtatb <> *blank
095600080930     c                   iter
095700080930     c                   endif
095800030701     c                   move      *blanks       v1csce
095900080930     c                   z-add     adtprg        vi2prg
096000081010     c                   z-add     adtprg        lastprg           3 0
096100030701      *data decorrenza
096200080930     c                   if        adtddt > 0
096300080930     c                   move      adtddt        dataiso
096400030701     c                   move      dataiso       dataeur
096500080930     c                   move      dataeur       vi2ddt
096600030701     c                   else
096700080930     c                   clear                   vi2ddt
096800030701     c                   endif
096900030701      *data scadenza
097000080930     c                   if        adtdst > 0
097100080930     c                   move      adtdst        dataiso
097200030701     c                   move      dataiso       dataeur
097300080930     c                   move      dataeur       vi2dst
097400030701     c                   else
097500080930     c                   clear                   vi2dst
097600030701     c                   endif
097700080930      *data scadenza
097800080930     c                   if        adtduv > 0
097900080930     c                   move      adtduv        dataiso
098000080930     c                   move      dataiso       dataeur
098100080930     c                   move      dataeur       vi2duv
098200080930     c                   else
098300080930     c                   clear                   vi2duv
098400080930     c                   endif
098500080930      *data scadenza
098600080930     c                   if        adtdcn > 0
098700080930     c                   move      adtdcn        dataiso
098800080930     c                   move      dataiso       dataeur
098900090324     c                   move      dataeur       vi2dcn
099000080930     c                   else
099100090324     c                   clear                   vi2dcn
099200080930     c                   endif
099300080930      *data scadenza
099400080930     c                   if        adtdts > 0
099500080930     c                   move      adtdts        dataiso
099600080930     c                   move      dataiso       dataeur
099700080930     c                   move      dataeur       vi2dts
099800080930     c                   else
099900080930     c                   clear                   vi2dts
100000080930     c                   endif
100100090402      *data scadenza
100200090402     c                   if        adtdrc > 0
100300090402     c                   move      adtdrc        dataiso
100400090402     c                   move      dataiso       dataeur
100500090402     c                   move      dataeur       vi2drc
100600090402     c                   else
100700090402     c                   clear                   vi2drc
100800090402     c                   endif
100900030701      *esco per raggiunta massima capacità sfl
101000030617     c                   if        snrr1 > 9990
101100030617     c                   leave
101200030617     c                   endif
101300030617
101400030604     C                   EVAL      snrr1 = snrr1 + 1
101500030604     C                   WRITE     video2
101600011228     C                   ENDDO
101700011228
101800030604     C                   IF        snrr1  > 0
101900020219     C                   EVAL      *IN91 = *ON
102000020219     C                   MOVE      1             OK                1 0
102100020219     C                   ELSE
102200020219     C                   MOVE      0             OK                1 0
102300030701     C                   ENDIF
102400030612     C                   EVAL      nrr1  = 1
102500011228     C                   ENDIF
102600020219     C                   EVAL      WrkCarS01 = *OFF
102700030604     C                   EVAL      snrr1  = 1
102800030604     C                   EVAL      csrrrn = 1
102900011228     C                   ENDSR
103000011227     c**********************************************************************
103100011227     c* gestione sfl
103200011227     c**********************************************************************
103300000613     C     GestioneSFL   BEGSR
103400000627      *
103500000627     C                   EVAL      WrkEofS01 = *OFF
103600000627      * F10 Immissione.
103700081013     C  n21              IF        *INKJ
103800080930     c                   clear                   video3
103900131021     c                   move      knmus         vknmus
104000131021     c                   move      knsif         vknsif
104100131021     c                   movel     ragut         rsut
104200081010     c                   seton                                        23
104300080930     c                   movel     vidfil        vadtpdr
104400080930     c                   move      vidpdr        vadtpdr
104500090522     c                   clear                   vadtpdrd
104600081001     c     kapd          chain     fiapd01l
104700090720     c                   if        %found(fiapd01l)
104800090507     c                   movel     apdrsf        vadtpdrd
104900081001     c                   endif
105000080930     c     vadtpdrk      setgt     fiadt01l
105100080930     c     vadtpdrk      readpe(n) fiadt01l
105200080930     c                   if        not %eof(fiadt01l)
105300090429     c                   if        adtdts = 0
105400090429     c                   eval      $msg = 'PROGRESSIVO PRECEDENTE NO-
105500090429     c                             N CONVALIDATO INSERIMENTO NON AMMESSO'
105600090429     c                   seton                                        2896
105700090429     C                   GOTO      NORIGA
105800090429     c                   endif
105900090612     c                   if        adtdcn = 0
106000090612     c                   exsr      srcdf
106100090612     c                   if        no_trs <> *blank
106200090612     c                   eval      $msg = 'Non trovato un contratto -
106300090612     c                             legato al fornitore inserimento  non ammesso'
106400090612     c                   seton                                        2896
106500090612     C                   GOTO      NORIGA
106600090612     c                   endif
106700090612     c                   endif
106800080930     c                   eval      vADTPRG =  ADTPRG + 1
106900081010     c                   move      adtdst        dataiso
107000081010     c                   move      dataiso       datacom
107100080930     c     datacom       adddur    1:*d          datacom
107200080930     c                   move      datacom       vadtddt
107300081009     c                   else
107400081009     c                   setoff                                       23
107500090114     c                   if        wuda > 20090131
107600081009     c                   z-add     wdat          vadtddt
107700081009     c                   movel     01            vadtddt
107800090114     c                   else
107900090114     c                   move      01022009      vadtddt
108000080930     c                   endif
108100090114     c                   endif
108200090114     c                   move      31122039      vadtdst
108300040728     c                   clear                   modalita
108400080930     c                   clear                   fiadt000
108500040728     c                   move      knmus         vknmus
108600040728     c                   move      knsif         vknsif
108700040728     c                   movel     ragut         rsut
108800040728     c                   exsr      Gesvideo3
108900081009     c                   seton                                        23
109000040728     c                   eval      wrkcars01 = *on
109100090429     c                   endif
109200040728     c                   if        csrrrn = 0
109300040728     c                   goto      noriga
109400040728     c                   endif
109500030605      * Elaborazione opzioni.
109600011228     c*
109700030605     c                   do        *hival        X                 4 0
109800030605     c     X             CHAIN     video2                             50
109900030605     C   50              LEAVE
110000030604     C                   IF        v1csce <> *blanks
110100030605     c                   exsr      opzione
110200030611
110300030611     c                   move      *blank        v1csce
110400030605     c                   update    video2
110500030611
110600011228     C                   ENDIF
110700030605     C                   enddo
110800040728      *
110900040728     c                   if        modalita = '1' or modalita = '3'
111000081007     c                             or modalita = '4' or modalita = '9'
111100090326     c                             or modalita = '2' or modalita = '7'
111200040728     c                   eval      wrkcars01 = *on
111300040728     c                   endif
111400000627      *
111500031003     C     noriga        ENDSR
111600090522      *--------------------------------------------------
111700090522     c     srcdf         begsr
111800090522      *--------------------------------------------------
111900090612     c                   clear                   no_trs            1
112000090522     c                   clear                   sogpartiva
112100090522     c                   clear                   sogdes
112200090522     c     kfrn          chain     anfrn01l
112300090522     c                   if        %found(anfrn01l)
112400090522     c     frnsogg       chain     ansog01l
112500090525     c                   if        %found(ansog01l)
112600090525     c                   movel     sogpartiva    ivak
112700090525     c     kaitrs2       chain     aitrs02l
112800090525     c                   if        %found(aitrs02l)
112900090525     c                   if        trsdec > adtddt
113000090525     c                   eval      $msg = 'Data decorrenza tariffa minore -
113100090525     c                             di data inizio contratto'
113200090525     c                   seton                                        2896
113300090525     c                   except    libera
113400090525     c                   endif
113500090612     c                   else
113600090612     c                   move      'X'           no_trs
113700090525     c                   endif
113800090525     c                   endif
113900090525     c                   endif
114000090522     c                   endsr
114100120220     C**************************************************************************
114200120220     C*    CONTROLLo per a/d se il fornitore è un trazionista certificato
114300120220     C**************************************************************************
114400120220     C     veriftraz     BEGSR
114500120510     c                   if        modalita = '2'
114600120510     C                   move      adtcsf        aAAcsf
114700120510     C                   move      *zeros        aAAcdf
114800120510     C                   move      adtcdf        aAAcdf
114900120510     c                   else
115000120510     C                   move      apdcsf        aAAcsf
115100120510     C                   move      *zeros        aAAcdf
115200120510     C                   move      apdksc        aAAcdf
115300120510     c                   endif
115400120220     c                   clear                   esito
115500120220     c                   clear                   sogpartiva
115600120321     c     kfrnAAA       chain     anfrn01l
115700120220     c                   if        %found(anfrn01l)
115800120220     c     frnsogg       chain     ansog01l
115900120220     c                   if        %found(ansog01l)
116000120518     c                   move      aaacsf        parsoc
116100120220     c                   movel     sogpartiva    pariva
116200120220     c                   call      'TRULTRAZ'
116300120220     c                   parm                    partraz
116400120306     c                   parm                    dsAIATS
116500120220     c                   endif
116600120220     c                   endif
116700120510      *se sono in modifica quindi data scadenza allegato e ho un errore di
116800120510      *congruenza con le date contratto al momento lo bypasso
116900120510     c                   if        modalita = '2' and
117000120717     c                             esito <> ' '
117100120605     c                   move      '1'           esito
117200120723     c                   endif
117300120220     c                   endsr
117400120723ab   C**************************************************************************
117500120723ab    * Controlla se nel periodo successivo è prevista un'altra società
117600120723ab   C**************************************************************************
117700120723ab   C     TEST_SOCieta  begSR
117800120723ab    *
117900120723ab    /FREE
118000120723ab
118100120723EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( adtpdrf
118200120723EDPDC                                             : dtVALIDITA
118300120723EDPDC                                             : 'O'
118400120723EDPDC                                             : valDatIniz
118500120723EDPDC                                             : valDatFine
118600120723ab                                                );
118700120723ab
118800120723EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
118900120723EDPDC    // Gestire l'errore.
119000120723ab     ENDIF;
119100120723ab
119200120723ab    /END-FREE
119300120724     c                   move      valdatfine    valdatfineW       8 0
119400120723ab   C                   ENDSR
119500011227     c**********************************************************************
119600000607     C     *INZSR        BEGSR
119700011227     c**********************************************************************
119800000607      *
119900000607     C     *ENTRY        PLIST
120000000607     C                   PARM                    Kpjba
120100030604     C                   Z-ADD     1             CODUT
120200030604     C                   CALL      'X§PARUT'
120300030604     C                   PARM                    UTEDSE
120400030604     C                   MOVEL     REC80         CNCR80
120500030703     C                   MOVEL     Ragut         rsut
120600030703     C                   MOVEL     knsif         vknsif
120700030703     C                   MOVEL     knmus         vknmus
120800030702      *reperimento data
120900030702     C                   TIME                    W0120            14 0
121000030702     C                   MOVE      W0120         WDAT              8 0
121100030702     C*
121200030702     C                   Z-ADD     WDAT          G02DAT
121300030702     C                   MOVEL     *BLANK        G02ERR
121400030702     C                   CALL      'XSRDA8'
121500030702     C                   PARM                    WLBDA8
121600030702     C* UDATE A 8 IN AAAA/MM/GG
121700030702     C                   Z-ADD     G02INV        WUDA              8 0
121800080930     C                   CLEAR                   Tibs36Ds
121900080930     C                   EVAL      I36ute = knmus
122000080930     C                   CALL      'TIBS36R'
122100080930     C                   PARM                    tibs36ds
122200030718
122300030718     C     Kapd          KLIST
122400030718     C                   KFLD                    tipoa             1
122500080930     C                   KFLD                    vadtpdrk          7 0
122600030718     C                   move      'D'           tipoa
122700160920     C     Ktgt          KLIST
122800160920     C                   KFLD                    adtpdr
122900160920     C                   KFLD                    simul             1
123000090327     C     Kaitra        KLIST
123100090327     C                   KFLD                    vadtpdrk          7 0
123200090327     C                   KFLD                    data0             8 0
123300090417      *
123400090525     C     Kaitrs2       KLIST
123500090525     C                   KFLD                    adtcsf
123600090525     C                   KFLD                    ivak             16
123700090525     c                   kfld                    data0
123800090525      *
123900090417     C     Ktab          KLIST
124000090417     C                   KFLD                    tblkut
124100090417     C                   KFLD                    tblcod
124200090417     C                   KFLD                    tblkey
124300111124     C     Ktbe          KLIST
124400111124     C                   KFLD                    tbecod
124500111124     C                   KFLD                    tbeke1
124600111124     C                   KFLD                    tbeke2
124700000607      *
124800080930     C     Kadt          KLIST
124900080930     C                   KFLD                    vadtpdrk
125000081001     C                   KFLD                    vi2prg
125100090417      *
125200090522     C     Kfrn          KLIST
125300090522     C                   KFLD                    adtcsf
125400090522     C                   KFLD                    adtcdf
125500120321      *
125600120321     C     KfrnAAA       KLIST
125700120321     C                   KFLD                    aAAcsf
125800120321     C                   KFLD                    aAAcdf
125900090610      *
126000090610     C     Katt          KLIST
126100090610     C                   KFLD                    vidfil
126200090610     C                   KFLD                    vadtdstg
126300090522      *
126400111124     c* tabbella VPO record CONT
126500111124     C                   movel     'VPO'         tbecod
126600111124     C                   MOVEL(P)  'CONT'        tbeke1
126700111124     C                   clear                   tbeke2
126800111124     C                   clear                   dvpocont
126900111124     C     KTbe          CHAIN     Tntbe01l
127000111124     c                   if        %found(tntbe01l)
127100111124     C                   movel     tbeuni        dvpocont
127200111124     c                   else
127300111124     c                   clear                   dvpocont
127400111124     c                   end
127500090417     c* reperisce gg min durata allegato
127600090417     C                   movel     '5A'          tblcod
127700090417     C                   MOVEL(P)  'AUT'         tblKEY
127800090417     c                   z-add     1             tblkut
127900090508     c                   z-add     0             giomin
128000090417     C     KTAB          CHAIN     TABEL00F
128100090417     c                   if        %found(tabel00f)
128200090417     C                   movel     tbluni        ds5Aaut
128300090417     c                   if        §5AGGTARA <> 0
128400090417     C                   z-add     §5AGGTARA     giomin            3 0
128500090417     c                   end
128600090417     c                   end
128700120220     c                   move      '3I'          tblcod
128800120220     c                   movel(p)  '1'           tblkey
128900120220     c     ktab          chain     tabel00f
129000120220     c                   if        %found(tabel00f)
129100120220     c                   movel     tbluni        ds3i
129200120220     c                   eval      soctraini = %subst(§3IBST: 7: 3)
129300120220     c                   endif
129400090417      *
129500090417     C                   ENDSR
129600000607      *---------------------------------------------------------------------
129700090325     Ofiadt000  E            libera
