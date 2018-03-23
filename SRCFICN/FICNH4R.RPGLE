000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200980522     F* tibs02r * -----------------------------------------*
000300980522     F*          Ricerca / controllo tabelle
000400000000     F*----------------------------------------------------*
000401160201     Fficnh4d   CF   E             WORKSTN
000402151020     F                                     SFILE(bs02s01:nrr2)
000500160201     Ftnsre02l  iF   E           K DISK
000600160201     Fndmov08l  iF   E           K DISK
000601160201     Fndiva01l  iF   E           K DISK
000602151020     Ftntbe01l  iF   E           K DISK
000603160201     Fanfrn01l  iF   E           K DISK
000604160201     Fansog01l  iF   E           K DISK
002900000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
003000000000     D KPJBA         E DS
003001160201     D dsre          E DS
003002160201     D dsrexaf       E DS
003003160201     D exsreuni        S                   like(sreuni)
003004160201     D Dataeur         S               D   DATFMT(*EUR) INZ
003005160201     D Dataiso         S               D   DATFMT(*ISO) INZ
004100971021     F*----------------------------------------------------*
004200971021      * INDICATORI
004300971021     c* 28   - emetto campo messaggi
004400971021     c* 30/31- comodo
004500120716     c* 41   - filtro attivo (F4=Ricerca)
004600971021     c* 70   - sfl control
004800120716     c* 72   - sflEnd
004801151020     c* 71   - sfldsp
007600971016     C*----------------------------------------------------*
007700000000     C     *ENTRY        PLIST                                                  *
007800000000     C                   PARM                    KPJBA                          *
008200890429     C                   Z-ADD     1             CODUT
008300000000     C                   CALL      'XPARUT'
008400000000     C                   PARM                    PARUT
008500000000     C                   MOVEL     RAGUT         RSUT             20
008501160201     C     Kfrn          klist
008502160201     c                   kfld                    sresoc
008503160201     c                   kfld                    srecdf
008507160201     C     Ktbe          klist
008508151020     c                   kfld                    cod               3
008509151020     c                   kfld                    ke1              15
008510160201     c                   move      'SRE'         cod
008511160201     C     Kndmov        KLIST                                                  *
008512160201     C                   KFLD                    sresoc                         *
008513160201     C                   KFLD                    movctb                         *
008514160201     C                   KFLD                    movkcc                         *
008515160201     C                   KFLD                    srecdf                         *
008516160201     C                   KFLD                    dataiso                         *
008517160201     C                   KFLD                    vnft
008518160201     C                   move      'CG'          movctb                         *
008519160201     C                   move      '000400'      movkcc                         *
008520160201     C     Kndiva        KLIST                                                  *
008521160201     C                   KFLD                    movsys                         *
008522160201     C                   KFLD                    movnrasreg                     *
008600971016     C*
009600980525     c* gestione
009700120716     c                   dou       *inKL = *on
009800980525     c                   exsr      SUB_gesbf
009900120716     c                   enddo
010700980522     c*
010900980526     C                   eval      *inlr = *on                                       *
022600120716     C*-------------------------------------------------------------------------*
022700151020     c     carsfl        BEGSR
022701160201     C*-------------------------------------------------------------------------*
022702160201     C*-------- CARICA SUBFILE ----------------------------*
023200120716     c*
023300151021     C                   Z-ADD     0             nrr2              4 0          *
023400151021     C                   Z-ADD     1             CURS2                          *
023401160201     c                   clear                   primo             1
023402160202     c                   clear                   wimpr             9 2
023500151021     c* pulisco il subfile
023600151021     c                   seton                                        70
023700151021     c                   write     bs02c01
023800151021     c                   setoff                                       70  72
023801160201     C     *loval        setll     tnsre02l                                     *
023802160201     c                   do        *hival
023803160201     C                   read      tnsre02l                                     *
023804160201     c                   if        %eof(tnsre02l)
023805160201     c                   leave
023806160201     c                   endif
023807160201      *fornitore e documento
023808160201     c                   if        sreuni <> exsreuni
023809160201     c                   if        primo  <> *blank
023810160202     c                   eval      wimpr = vimpr * -1
023811160202     c                   if        vimpf  <> Wimpr
023812160201     C                   ADD       1             nrr2                           *
023813160201     C                   WRITE     bs02s01                                      *
023814160202     c                   endif
023815160201     c                   endif
023816160201     c                   exsr      riempi
023817160201     c                   else
023818160201     c                   add       sretim        vimpr
023819160201     c                   endif
027100971015     C                   ENDdo                                                  *
027200160201     c*ultima riga
027201160202     c                   eval      wimpr = vimpr * -1
027202160202     c                   if        vimpf <> wimpr
027203160201     C                   ADD       1             nrr2                           *
027204160201     C                   WRITE     bs02s01                                      *
027205160201     c                   endif
027206160201      *
027300120716     c                   eval      *in72 = *on
027500980525     c*
027600980525     c                   endsr
027601160201     C*-------------------------------------------------------------------------*
027602160201     c* riempimento campi SFL
027603160201     C*-------------------------------------------------------------------------*
027604160201     c     riempi        begsr
027605160201     c                   move      'X'           primo
027606160201     c                   movel     sreuni        exsreuni
027607160201     c                   movel     sreuni        dsrexaf
027608160201     c                   move      §SREXAFDF     dataiso
027609160201     c                   move      dataiso       dataeur
027610160201     c                   move      dataeur       vdft
027611160201     c                   move      §SREXAFNF     vnft
027612160201     c                   move      sresre        vsre
027613160201     c                   move      sresoc        vsoc
027614160201     c                   move      srecdf        vfor
027615160201     c                   z-add     sretim        vimpr
027616160201      *fornitore
027617160201     c                   clear                   vford
027618160201     c                   if        vfor <> *blank
027619160201     C     Kfrn          CHAIN     anfrn01l
027620160201     c                   if        %found(anfrn01l)
027621160201     c     frnsogg       chain     ansog01l
027622160201     c                   if        %found(ansog01l)
027623160201     c                   movel     sogdes        vford
027624160201     c                   endif
027625160201     c                   endif
027626160201     c                   endif
027627160201      *causale
027628160201     c                   movel     vsre          ke1
027629160201     c                   clear                   dsre
027630160201     c     ktbe          chain     tntbe01l
027631160201     c                   if        %found(tntbe01l)
027632160201     c                   movel     tbeuni        dsre
027633160201     c                   endif
027634160201     c                   movel     §sredes       vsred
027635160201      *documento anticipo
027636160201     c     kndmov        chain     ndmov08l
027637160201     c                   if        %found(ndmov08l)
027638160201     c     kndiva        chain     ndiva01l
027639160201     c                   if        %found(ndiva01l)
027640160201     c                   z-add     ivaimponib    vimpf
027641160201     c                   endif
027642160201     c                   endif
027643160201     c                   endsr
027700980525     C*-------------------------------------------------------------------------*
027800980525     c* SUB_GESBF Routine Gestione Subfile
027900980525     C*-------------------------------------------------------------------------*
028000980525     c     SUB_GESBF     BEGSR
028100980525     C*
028200971015     C* nessun record caricato
028201151020     c                   exsr      carsfl
028300160202    1c                   if        nrr2 = 0
028302160202     C                   WRITE     bs02t01                                      *
028303160202     C                   EXFMT     bs02w01                                      *
028304160201     c   kl              leavesr
028400151020     c                   eval      *in71 = *off
028900980526    xc                   else
029000151020     c                   eval      *in71 = *on
029100980526    1c                   endif
029200000000     C*
029300980526     C                   WRITE     bs02z01                                      *
029400980526     C                   WRITE     bs02t01                                      *
029500981030?   1C                   do        *hival                                       *
029600971015     c* sfl controlo
029700980526     C                   EXFMT     bs02c01                                      *
029900981030     c* F12= fine lavoro
030000120716    2C                   if        *inkl = *on
030200120716     c                   leavesr
030300120716    2c                   endif
035000120716     c*
035100120716?   1C                   enddo                                                  *do  hival
035200971021     c**
035300971016     c                   endsr
