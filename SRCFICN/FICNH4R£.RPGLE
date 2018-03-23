000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200980522     F* tibs02r * -----------------------------------------*
000300980522     F*          Ricerca / controllo tabelle
000400000000     F*----------------------------------------------------*
000401151020     Fycostp7d  CF   E             WORKSTN
000402151020     F                                     SFILE(bs02s01:nrr2)
000500151020     Ffnfna01l  iF   E           K DISK
000600151020     Ffntna01l  iF   E           K DISK
000601151020     Fazorg01l  iF   E           K DISK
000602151020     Ftntbe01l  iF   E           K DISK
000603151020     Fcnaco00f  iF   E           K DISK
002900000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
003000000000     D KPJBA         E DS
003001151020     D dtce          E DS
003002151020     D Dataw           S               D   DATFMT(*EUR) INZ
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
007801151020     c                   movel     kpjbu         prich             9 0
008200890429     C                   Z-ADD     1             CODUT
008300000000     C                   CALL      'XPARUT'
008400000000     C                   PARM                    PARUT
008500000000     C                   MOVEL     RAGUT         RSUT             20
008501151020     C     Kaco          klist
008502151020     c                   kfld                    ACOKUT
008503151020     c                   kfld                    ACOKCC
008504151020     c                   kfld                    vksc
008505151020     c                   z-add     1             ACOKUT
008506151020     c                   z-add     151           ACOKCC
008507151020     C     Ktce          klist
008508151020     c                   kfld                    cod               3
008509151020     c                   kfld                    ke1              15
008510151020     c                   move      'TCE'         cod
008600971016     C*
009600980525     c* gestione
009700120716     c                   dou       *inKL = *on
009800980525     c                   exsr      SUB_gesbf
009900120716     c                   enddo
010700980522     c*
010900980526     C                   eval      *inlr = *on                                       *
022300120716     C*-------------------------------------------------------------------------*
022400980525     C*-------- CARICA SUBFILE ----------------------------*
022500981030     C*                        SISTEMA INFORMATIVO  VALORIZZATO come da testata tabella
022600120716     C*-------------------------------------------------------------------------*
022700151020     c     carsfl        BEGSR
023200120716     c*
023300151021     C                   Z-ADD     0             nrr2              4 0          *
023400151021     C                   Z-ADD     1             CURS2                          *
023500151021     c* pulisco il subfile
023600151021     c                   seton                                        70
023700151021     c                   write     bs02c01
023800151021     c                   setoff                                       70  72
023801151021     C     prich         chain     fntna01l                                     *
023802151021     c                   if        %found(fntna01l)
023803151021      *filiali
023804151021     C                   MOVE      tNAFILa       vfila                          *
023805151021     c     tnafila       chain     azorg01l
023806151021     c                   movel     orgdes        vfilad
023807151021     C                   MOVE      tNAFILd       vfild                          *
023808151021     c     tnafild       chain     azorg01l
023809151021     c                   movel     orgdes        vfildd
023810151021      * clienti
023811151021     C                   MOVE      tNAksc        vksc                           *
023812151021     c                   clear                   vkscd
023813151021     c                   if        vksc > 0
023814151021     c     kaco          chain     cnaco00f
023815151021     c                   movel     acorag        vkscd
023816151021     c                   endif
023817151021      * causale
023818151020     C                   MOVE      tNAcaus       vnota                          *
023819151021     C                   MOVEl     tNAcaus       ke1
023820151020     c                   clear                   vnotad
023821151020     c     ktce          chain     tntbe01l
023822151020     c                   if        %found(tntbe01l)
023823151020     C                   MOVEl     tbeuni        dtce                           *
023824151020     c                   movel     §tcedes       vnotad
023825151020     c                   endif
023826151020      * fase
023827151020     c                   clear                   vfaset
023828151020     c                   clear                   vfasetd
023829151020     c                   if        tnafase = 100
023830151020     c                   EVAL      VFASEt = tnafase
023831151020     c                   EVAL      VFASEtD = 'CONTABILIZZATO'
023832151020     c                   endif
023833151020      *
023834151020     C                   MOVE      tnadtac       dataw                          *
023835151020     C                   MOVE      dataw         vdatc                          *
023836151020     C                   z-add     tNAimp        vimp                           *
023837151020     c                   endif
023900020211      *
024000020211      * Posizionamento a seconda delle chiavi passate
024200151020     C     prich         SETLL     fnfna01l                                     *
024600020211      *
024601151020     c                   do        *hival
024700151020     C     prich         READE     fnfna01l
024800000000     C*
025000151020     c                   if        %eof(fnfna01l)
025100151020     c                   leave
025200151020     c                   endif
025800151020     c                   move      FNAANN        vann
025900151020     C                   MOVE      FNAFIL        vfil                           *
026000151020     C                   MOVE      FNAFASE       vfase                          *
026001151020     c                   select
026002151020     c                   when      vfase = 10
026003151020     c                   EVAL      VFASED =  'IMMESSO'
026004151020     c                   when      vfase = 20
026005151020     c                   eval      VFASED  = 'SOSPESO'
026006151020     c                   when      vfase = 30
026007151028     c                   eval      VFASED = 'VERIFICATO'
026010151020     c                   endsl
026100151020     C                   MOVEL     FNADTAAG      dataw                          *
026101151020     C                   MOVE      dataw         vdatag                         *
026200151020     C                   MOVEL     FNAORAAG      voraag                         *
026400151020     C                   MOVE      FNAUTE        vuten                          *
026401151020     C                   ADD       1             nrr2                           *
026500980526     C                   WRITE     bs02s01                                      *
027100971015     C                   ENDdo                                                  *
027200120716     c*
027300120716     c                   eval      *in72 = *on
027500980525     c*
027600980525     c                   endsr
027700980525     C*-------------------------------------------------------------------------*
027800980525     c* SUB_GESBF Routine Gestione Subfile
027900980525     C*-------------------------------------------------------------------------*
028000980525     c     SUB_GESBF     BEGSR
028100980525     C*
028200971015     C* nessun record caricato
028201151020     c                   exsr      carsfl
028300980526    1c                   if        nrr2 = 0
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
