000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300020131      * TNSC04R - stampa lettere per contrassegni omessi             *
001200900606?     *--------------------------------------------------------------*
001500020131     FTNCSB03L  IF   E           K DISK
001700011031     FTNCSA01L  IF   E           K DISK
001900910702     FAZORG01L  IF   E           K DISK
002000011031     Ftitas30c  IF   E           K DISK
002001121025     ftabel00f  if   e           k disk
002200121025     Ftnsc04MP  O    e             PRINTER OFLIND(*IN22) usropn
002201121025     Ftnsc04Me  O    e             PRINTER OFLIND(*IN23) usropn
005200900515      *
006000011031     d*
006001020131     D TIBS69        E DS                  EXTNAME(TIBS69DS)
006200011031     D CNACO         E DS                  EXTNAME(CNACO00F)
006300011031     D CNIND         E DS                  EXTNAME(CNIND00F)
006400011031     D CNCLP         E DS                  EXTNAME(CNCLP00F)
006500011031     D FNCLS         E DS                  EXTNAME(FNCLS00F)
006502020207     D tibs20DS      E DS
006503020131     D KPJBA         E DS
006504121025     D DS5A          e ds
006505121025     D og143         e ds
006506020207     D com11           S             11
006507020207     D W0140           S             14  0
006508020207     D W0080           S              8  0
006509020207     D WDTGIO          S               D   DATFMT(*EUR)
006510020207     D loc             S                   like(orgloc)
006511110428     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
006512110428     D ESITO_OK...
006513110428     D                 C                   0
006514110428     ***************************************************************************
006515110428     **
006516110428     ** Dichiarazione prototipi procedure esterne.
006517110428     **
006518110428     ***************************************************************************
006519110428      /DEFINE DFTACTGRP_YES
006520110428     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
006521110428      /UNDEFINE DFTACTGRP_YES
006522110428     ***************************************************************************
006523110428     **
006524110428     ** Definizione strutture dati.
006525110428     **
006526110428      **************************************************************************
006527110428     D tibsSocI0...
006528110428     D               E DS                  QUALIFIED
006529110428     D                                     INZ
006530110428     D tibsSocO0...
006531110428     D               E DS                  QUALIFIED
006532110428     D                                     INZ
006533110428     ***************************************************************************
006534110428     **
006535110428     ** Definizione variabili modulo/programma.
006536110428     **
006537110428     ***************************************************************************
006538110428     D prmRqsOpCode...
006539110428     D                 S             10A
006540110428     D prmRpyOpCode...
006541110428     D                 S             10A
006542110428     D prmRpyIdMsg...
006543110428     D                 S             10I 0
006544110428     D prmRqsFormato...
006545110428     D                 S             10A
006546110428     D prmRqsDataSize...
006547110428     D                 S             10I 0
006548110428     D prmRpyFormato...
006549110428     D                 S             10A
006550110428     D prmRpyDataSize...
006551110428     D                 S             10I 0
006600011031     ***********************************************************************
011201020131     c* reperisco campi per stampa
011202020131     c                   exsr      srcampi
011203020207     C* STAMPO LETTERA MITTENTE contrassegni omessi
011204121025     c
011205121025     c                   if        §ogntw = 'DPD' or
011206121025     c                             §ogntw = 'FED' or
011207121025     c                             §ogntw = 'EEX'
011208121025     C                   write     TES01e
011209121025     c                   else
011210020207     C                   write     TES01
011211121025     c                   endif
011218110428     ** Chiudo il programma.
011219110428     C                   CALL      'TIBSSOCR'
011220110428     C                   PARM      'FINALIZE'    prmRqsOpCode
011221110428     C                   PARM                    prmRpyOpCode
011222110428     C                   PARM                    prmRpyIdMsg
011300910521      *
011500910521     C                   SETON                                        LR
011600011031     C**********************************************************************
011700020131     C     *INZSR        BEGSR
011800011031     C**********************************************************************
011900000000     C     *ENTRY        PLIST
012000000000     C                   PARM                    KPJBA
012100020131     C                   MOVEL     KPJBU         Tibs20ds
012106110428     c                   exsr      Decod_societa
014200971230      *  Data del giorno
014300020207     C                   TIME                    W0140
014400020207     C                   MOVE      W0140         W0080
014401020207      * UDATE IN GGMMAAAA
014402020207     C                   MOVE      W0080         WDTGIO
018600900515      * CHIAVI                                                        *
021100020131     C     KSPE          KLIST
021200020131     C                   KFLD                    I20aas
021300020131     C                   KFLD                    I20lnp
021400020131     C                   KFLD                    I20nrs
021500020131     C                   KFLD                    I20nsp
021501121025      *
021502121025     c     ktab          klist
021503121025     c                   kfld                    tblkut
021504121025     c                   kfld                    tblcod
021505121025     c                   kfld                    tblkey
021506121025
021507121025     c* aggancio tabella 5A per numero gg tentato recupero contrassegno
021508121025     c                   move      1             tblkut
021509121025     c                   movel     '5A'          tblcod
021510121025     c                   movel(p)  '1'           tblkey
021511121025     c     ktab          chain     tabel00f
021512121025     c                   if        %found(tabel00f)
021513121025     c                   movel     tbluni        ds5a
021514121025     c                   else
021515121025     c                   Z-ADD     999           §5Agrc
021516121025     c                   Z-ADD     999           §5Al2
021517121025     c                   end
021518121025     c     i20lna        chain     azorg01l
021519121025     c                   if        %found(azorg01l)
021520121025     C                   MOVEL     ORGDE3        og143
021521121025     c                   if        §ogntw = 'DPD' or
021522121025     c                             §ogntw = 'FED' or
021523121025     c                             §ogntw = 'EEX'
021524121025     c                   open      tnsc04me
021525121025     c                   z-add     §5al2         gios
021526121025     c                   else
021527121025     c                   open      tnsc04mp
021528121025     c                   z-add     §5agrc        gios
021529121025     c                   endif
021530121025     c                   endif
021531121025     c
022000910701      *
024400910521     C                   ENDSR
044200011031      *-----------------------------------------------------***********
044300011031      * Campi per stampa
044400011031      *-----------------------------------------------------***********
044500011031     C     SRCAMPI       BEGSR
044502020207     c     kSPE          chain     TNCSB03L
044503020207     c                   if        %found(tncsb03l)
044504020207     C                   MOVE      CSBLNP        SLNP
044505020207     C                   MOVE      CSBAAS        SAAS
044508020207     C                   MOVE      CSBNRS        SNRS
044509020207     C                   MOVE      CSBNSP        SNSP
044510020207     C                   MOVE      CSBMGS        WDUE              2 0
044511020207     C                   MOVEL     WDUE          SGMSP
044512020207     C                   MOVEL     CSBMGS        WDUE
044513020207     C                   MOVE      WDUE          SGMSP
044514020207     C                   MOVEL     csbvca        Sdiv
044517020207     C                   Z-ADD     CSBCAS        SASS
044520020207     c                   end
044600011031     c* destinatario spedizione
044700011106     c                   clear                   prom              5
044800020131     c     kspe          chain     titas30c
044900011031     c                   if        %found(titas30c)
045100011031     C                   MOVEL     tasrsd        ragdo
045500011031     c                   else
045700011031     C                   MOVEL     *ALL'?'       ragdo
046100011031     c                   end
046200011031     c* punto operativo di partenza
046300020131     C     csblnp        CHAIN     AZORG01L
046400011031     c                   if        %found(azorg01l)
046500011031     C                   MOVEL     ORGDES        pom
046501020207     C                   MOVEL     ORGDES        pomo
046502121018     C                   MOVEL     ORGloc        loc
046600011031     c                   else
046701020207     C                   MOVEL     *ALL'?'       pom
046702020207     C                   MOVEL     *ALL'?'       pomo
046703121018     C                   MOVEL     *ALL'?'       loc
046800011031     c                   end
046900011031     c* punto operativo di arrivo
047000020131     C     csblna        CHAIN     AZORG01L
047100011031     c                   if        %found(azorg01l)
047200011031     C                   MOVEL     ORGDES        pod
047500011031     c                   else
047600011031     C                   MOVEL     *ALL'?'       pod
047900011031     c                   end
048000011031     c* mittente spedizione
048100011031     C                   MOVE      csbCDI        §8899             4
048200011031     c                   if        §8899 = '8888' or §8899 = '9999'
048300020131     c     kspe          chain     tncsa01l
048400011031     c                   if        %found(tncsa01l)
048600011031     C                   MOVEL     csarsd        ragmo
049000011031     c                   else
049200011031     C                   MOVEL     *ALL'?'       ragmo
049600011031     c                   end
049700011031     c                   else
049800011031     C                   CLEAR                   TIBS69
049900011031     C                   CLEAR                   CNACO
050000011031     C                   cLEAR                   CNIND
050100011031     C                   CLEAR                   CNCLP
050200011031     C                   CLEAR                   FNCLS
050300011031     C                   MOVEL     ' '           I69TLA
050400011031     C                   MOVEL     KNSIF         I69SIF
050500011031     C                   Z-ADD     0             I69KCC
050600011031     C                   Z-ADD     csbcdi        I69KAC
050700011031     C                   Z-ADD     csbcdi        I69KIN
050800011031     C                   Z-ADD     0             I69KCP
050900011031     C                   CALL      'TIBS69R'
051000011031     C                   PARM                    TIBS69
051100011031     C                   PARM                    CNACO
051200011031     C                   PARM                    CNIND
051300011031     C                   PARM                    CNCLP
051400011031     C                   PARM                    FNCLS
051500011031     C                   IF        o69err = *BLANK
051700011106     C                   eval      ragmo = acorag
052100011031     C                   END
052200011031     c                   end
052201020207     c                   move      wdtgio        com11
052202121022     c*m                 move      wdtgio        sdtcom
052204020207     C* Altri campi
052205020208     C                   eval      cdtpom = %trimr(loc) + ',' + com11
053200900515     C                   ENDSR
053300110428     C**************************************************************************
053400110428     C* decodifica la società
053500110428     C************************************************************
053600110428     C     Decod_societa begSR
053601110428     ** Inizializzo il programma.
053602110428     C                   CALL      'TIBSSOCR'
053603110428     C                   PARM      'INIT'        prmRqsOpCode
053604110428     C                   PARM                    prmRpyOpCode
053605110428     C                   PARM                    prmRpyIdMsg
053700110428      *
053800110428     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
053900110428     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
054000110428     c* forzo società 201
054100110428     C                   EVAL      tibsSocI0.idSocieta ='201'
054200110428     C
054300110428     C                   CALL      'TIBSSOCR'
054400110428     C                   PARM      'GETANAGRAF'  prmRqsOpCode
054500110428     C                   PARM      *BLANK        prmRpyOpCode
054600110428     C                   PARM      *ZERO         prmRpyIdMsg
054700110428     C                   PARM      'TIBSSOCI0'   prmRqsFormato
054800110428     C                   PARM                    tibsSocI0
054900110428     C                   PARM                    prmRqsDataSize
055000110428     C                   PARM      'TIBSSOCO0'   prmRpyFormato
055100110428     C                   PARM                    tibsSocO0
055200110428     C                   PARM                    prmRpyDataSize
055300110428     c                   if         PRMRPYIDMSG >= 0
055400110428     c                              and TIBSSOCO0.IDSOCIETA <> *blank
055500110428     c                   eval       ragsoc  = tibsSocO0.RAGSOCIALE
055600110428     c                   else
055700110428     c                   eval       ragsoc  = *all'?'
055800110428     c                   end
055900110428      *
056000110428     C                   ENDSR
