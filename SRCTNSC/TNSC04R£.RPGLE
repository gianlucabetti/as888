000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300020131      * TNSC04R - stampa lettere per contrassegni omessi             *
001200900606?     *--------------------------------------------------------------*
001500020131     FTNCSB03L  IF   E           K DISK
001700011031     FTNCSA01L  IF   E           K DISK
001900910702     FAZORG01L  IF   E           K DISK
002000011031     Ftitas30c  IF   E           K DISK
002200020207     Ftnsc04MP  O    e             PRINTER OFLIND(*IN23)
002201020207     Ftnsc04DP  O    e             PRINTER OFLIND(*IN24)
005200900515      *
006000011031     d*
006001020131     D TIBS69        E DS                  EXTNAME(TIBS69DS)
006200011031     D CNACO         E DS                  EXTNAME(CNACO00F)
006300011031     D CNIND         E DS                  EXTNAME(CNIND00F)
006400011031     D CNCLP         E DS                  EXTNAME(CNCLP00F)
006500011031     D FNCLS         E DS                  EXTNAME(FNCLS00F)
006502020207     D tibs20DS      E DS
006503020131     D KPJBA         E DS
006504020207     D com11           S             11
006505020207     D W0140           S             14  0
006506020207     D W0080           S              8  0
006507020207     D WDTGIO          S               D   DATFMT(*EUR)
006508020207     D loc             S                   like(orgloc)
006509110428     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
006510110428     D ESITO_OK...
006511110428     D                 C                   0
006512110428     ***************************************************************************
006513110428     **
006514110428     ** Dichiarazione prototipi procedure esterne.
006515110428     **
006516110428     ***************************************************************************
006517110428      /DEFINE DFTACTGRP_YES
006518110428     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
006519110428      /UNDEFINE DFTACTGRP_YES
006520110428     ***************************************************************************
006521110428     **
006522110428     ** Definizione strutture dati.
006523110428     **
006524110428      **************************************************************************
006525110428     D tibsSocI0...
006526110428     D               E DS                  QUALIFIED
006527110428     D                                     INZ
006528110428     D tibsSocO0...
006529110428     D               E DS                  QUALIFIED
006530110428     D                                     INZ
006531110428     ***************************************************************************
006532110428     **
006533110428     ** Definizione variabili modulo/programma.
006534110428     **
006535110428     ***************************************************************************
006536110428     D prmRqsOpCode...
006537110428     D                 S             10A
006538110428     D prmRpyOpCode...
006539110428     D                 S             10A
006540110428     D prmRpyIdMsg...
006541110428     D                 S             10I 0
006542110428     D prmRqsFormato...
006543110428     D                 S             10A
006544110428     D prmRqsDataSize...
006545110428     D                 S             10I 0
006546110428     D prmRpyFormato...
006547110428     D                 S             10A
006548110428     D prmRpyDataSize...
006549110428     D                 S             10I 0
006600011031     ***********************************************************************
011201020131     c* reperisco campi per stampa
011202020131     c                   exsr      srcampi
011203020207     C* STAMPO LETTERA MITTENTE contrassegni omessi
011204020207     C                   write     TES01
011205020207     C* STAMPO LETTERA DESTINATARIO contrassegni omessi
011206020208     c* 44 indicatore per scrivere la dicitura di RACCOMANDATA solo nella
011207020208     c* lettera del destinatario
011208020208     c                   seton                                        44
011209020207     C                   write     TES02
011211020207     C* STAMPO DICHIARAZIONE AUTOTRASPORTATORE contrassegni omessi
011212020207     C                   write     TES03
011213020208     c* stampo altre 3 lettere senza dicitura raccomandata e senza dichiarazione
011214020208     c                   setoff                                       44
011215020208     c                   do        3
011216020208     c                   write     tes02
011217020208     c                   enddo
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
044515020207     C                   MOVEL     csbvca        Sdiv1
044516020207     C                   MOVEL     csbvca        Sdiv2
044517020207     C                   Z-ADD     CSBCAS        SASS
044518020207     C                   Z-ADD     CSBCAS        SASS1
044519020207     C                   Z-ADD     CSBCAS        SASS2
044520020207     c                   end
044600011031     c* destinatario spedizione
044700011106     c                   clear                   prom              5
044800020131     c     kspe          chain     titas30c
044900011031     c                   if        %found(titas30c)
045000011031     C                   MOVEL     tasrsd        ragd
045100011031     C                   MOVEL     tasrsd        ragdo
045200011031     C                   MOVEL     tasind        indd
045300011031     C                   MOVEL     tascad        capd
045400011107     C                   eval      locd = %trimr(taslod)+' ('+tasprd+')'
045500011031     c                   else
045600011031     C                   MOVEL     *ALL'?'       ragd
045700011031     C                   MOVEL     *ALL'?'       ragdo
045800011031     C                   MOVEL     *all'?'       indd
045900011031     C                   MOVEL     *all'?'       capd
046000011031     C                   MOVEL     *ALL'?'       locd
046100011031     c                   end
046200011031     c* punto operativo di partenza
046300020131     C     csblnp        CHAIN     AZORG01L
046400011031     c                   if        %found(azorg01l)
046500011031     C                   MOVEL     ORGDES        pom
046501020207     C                   MOVEL     ORGDES        pomo
046502020207     C                   MOVEL     ORGloc        loc
046600011031     c                   else
046700020207     C                   MOVEL     *ALL'?'       loc
046701020207     C                   MOVEL     *ALL'?'       pom
046702020207     C                   MOVEL     *ALL'?'       pomo
046800011031     c                   end
046900011031     c* punto operativo di arrivo
047000020131     C     csblna        CHAIN     AZORG01L
047100011031     c                   if        %found(azorg01l)
047200011031     C                   MOVEL     ORGDES        pod
047300020207     C                   MOVEL     ORGDES        podo
047301050505     C                   MOVEL     ORGloc        locP
047302050505     C                   MOVEL     ORGIND        INDP
047303050505     C                   MOVE      ORGcpf        capP
047304050505     C                   MOVEl     ORGpro        proP
047305050505     C                   MOVEl     ORGtel        telP
047306050505     C                   MOVEl     ORGfax        faxP
047500011031     c                   else
047600011031     C                   MOVEL     *ALL'?'       pod
047700020207     C                   MOVEL     *ALL'?'       podo
047701050505     C                   MOVEL     *ALL'?'       locP
047702050505     C                   MOVEL     *ALL'?'       INDP
047703050505     C                   clear                   capP
047704050505     C                   MOVEl     *ALL'?'       proP
047705050505     C                   MOVEl     *ALL'?'       telP
047706050505     C                   MOVEl     *ALL'?'       faxP
047900011031     c                   end
048000011031     c* mittente spedizione
048100011031     C                   MOVE      csbCDI        §8899             4
048200011031     c                   if        §8899 = '8888' or §8899 = '9999'
048300020131     c     kspe          chain     tncsa01l
048400011031     c                   if        %found(tncsa01l)
048500011031     C                   MOVEL     csarsd        ragm
048600011031     C                   MOVEL     csarsd        ragmo
048700011031     C                   MOVEL     csavid        indm
048800011031     C                   MOVEL     csacad        capm
048900011107     C                   eval      locm = %trimr(csacid)+' ('+csaprm+')'
049000011031     c                   else
049100011031     C                   MOVEL     *ALL'?'       ragm
049200011031     C                   MOVEL     *ALL'?'       ragmo
049300011031     C                   MOVEL     *all'?'       indm
049400011031     C                   MOVEL     *all'?'       capm
049500011031     C                   MOVEL     *ALL'?'       locm
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
051600011031     C                   eval      ragm = acorag
051700011106     C                   eval      ragmo = acorag
051800011031     C                   eval      indm = indvia
051900011031     C                   MOVEL     INDCAP        CAPm
052000011107     C                   eval      locm = %trimr(indcit)+' ('+indprv+')'
052100011031     C                   END
052200011031     c                   end
052201020207     c                   move      wdtgio        com11
052202020208     c                   move      wdtgio        sdtcom
052204020207     C* Altri campi
052205020208     C                   eval      cdtpom = %trimr(loc) + ',' + com11
052206020208     C                   eval      cdtpod = %trimr(orgloc) + ',' + com11
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
