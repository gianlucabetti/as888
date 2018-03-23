000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200160304     F*----------------------------------------------------*
000300160304     F*          Rettifiche elenco
000400000000     F*----------------------------------------------------*
000500160304     Fficn47d1  CF   E             WORKSTN
000600151020     F                                     SFILE(bs02s01:nrr2)
000700160304     Ffifre03l  iF   E           K DISK
000800151020     Ftntbe01l  iF   E           K DISK
000900160304     Ffiapd01l  iF   E           K DISK
001000000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001100000000     D KPJBA         E DS
001200160304     D dcre          E DS
001300160304     D ficn47i       E DS
001400160304     D Dataeur         S               D   DATFMT(*EUR) INZ
001500160304     D Dataiso         S               D   INZ
001600971021     F*----------------------------------------------------*
001700971021      * INDICATORI
001800971021     c* 28   - emetto campo messaggi
001900971021     c* 30/31- comodo
002000120716     c* 41   - filtro attivo (F4=Ricerca)
002100971021     c* 70   - sfl control
002200120716     c* 72   - sflEnd
002300151020     c* 71   - sfldsp
002400971016     C*----------------------------------------------------*
002500000000     C     *ENTRY        PLIST                                                  *
002600000000     C                   PARM                    KPJBA                          *
002700160304     c                   movel     kpjbu         ficn47i
002800890429     C                   Z-ADD     1             CODUT
002900000000     C                   CALL      'XPARUT'
003000000000     C                   PARM                    PARUT
003100000000     C                   MOVEL     RAGUT         RSUT             20
003200160304     C     Ktbe          klist
003300151020     c                   kfld                    cod               3
003400151020     c                   kfld                    ke1              15
003500160304     c                   move      'CRE'         cod
003600160304     C     Kapd          klist
003700160304     c                   kfld                    freTIP
003800160304     c                   kfld                    frePDR
003900971016     C*
004000980525     c* gestione
004100120716     c                   dou       *inKL = *on
004200980525     c                   exsr      SUB_gesbf
004300120716     c                   enddo
004400980522     c*
004500980526     C                   eval      *inlr = *on                                       *
004600120716     C*-------------------------------------------------------------------------*
004700980525     C*-------- CARICA SUBFILE ----------------------------*
004800120716     C*-------------------------------------------------------------------------*
004900151020     c     carsfl        BEGSR
005000120716     c*
005100151021     C                   Z-ADD     0             nrr2              4 0          *
005200151021     C                   Z-ADD     1             CURS2                          *
005300151021     c* pulisco il subfile
005400151021     c                   seton                                        70
005500151021     c                   write     bs02c01
005600151021     c                   setoff                                       70  72
005700020211      *
005800020211      * Posizionamento a seconda delle chiavi passate
005900160304     C     c47dat1       SETLL     fifre03l                                     *
006000020211      *
006100151020     c                   do        *hival
006200160304     C                   READ      fifre03l
006300000000     C*
006400160304     c                   if        %eof(fifre03l) or freddc > c47dat2
006500151020     c                   leave
006600151020     c                   endif
006700160309      * note maggiorni di uno scartate
006800160309     c                   if        frepno > 1
006900160309     c                   iter
007000160309     c                   endif
007100160309      *filtri
007200160304     c                   movel     frepdr        filpdr            3 0
007300160309      *filliale
007400160304     c                   if        c47fil > 0 and c47fil <> filpdr
007500160304     c                   iter
007600160304     c                   endif
007700160309      *causale
007800160304     c                   if        c47cau <> *blank and c47cau <> frecre
007900160304     c                   iter
008000160304     c                   endif
008100160309      *tipo anagrafico
008200160309     c                   if        c47ana <> *blank and c47ana <> fretip
008300160309     c                   iter
008400160309     c                   endif
008500160309      *tipo servizio
008600160309     c                   if        c47tsr <> *blank and c47tsr <> fretsr
008700160309     c                   iter
008800160309     c                   endif
008900160309      * causale decodifica
009000160304     c                   if        frecre <> vcre
009100160304     C                   MOVEl(p)  frecre        ke1
009200160304     C                   MOVE      frecre        vcre
009300160304     c                   clear                   vcred
009400160304     c     ktbe          chain     tntbe01l
009500160304     c                   if        %found(tntbe01l)
009600160304     C                   MOVEl     tbeuni        dcre                           *
009700160304     c                   movel     §crdes        vcred
009800160304     c                   endif
009900160304     c                   endif
010000160304     c                   if        frepdr <> vpdr
010100160304     C                   MOVE      frepdr        vpdr
010200160304     c                   clear                   vpdrd
010300160304     c     kapd          chain     fiapd01l
010400160304     c                   if        %found(fiapd01l)
010500160304     c                   movel     apdrsc        vpdrd
010600160304     c                   endif
010700160304     c                   endif
010800160304     C                   MOVE      fretsr        vtsr                           *
010900160304     C                   MOVE      fretip        vana                           *
011000160304     C                   z-add     fretim        vimp                           *
011100160309     c                   if        frenot <> *blank
011200160309     c                   seton                                        15
011300160309     C                   MOVEl     frenot        vnote                          *
011400160309     c                   else
011500160309     c                   setoff                                       15
011600160309     c                   clear                   vnote
011700160309     c                   endif
011800160304     C                   MOVEL     freddc        dataiso                        *
011900160304     C                   MOVE      dataiso       dataeur                        *
012000160304     C                   MOVEL     dataeur       vddc                           *
012100160304      *raggiunto limite di carico parzializzazione evidentemente errata
012200160304      * forzo uscita
012300160304     c                   if        nrr2 = 9998
012400160304     c                   leave
012500160304     c                   endif
012600151020     C                   ADD       1             nrr2                           *
012700980526     C                   WRITE     bs02s01                                      *
012800971015     C                   ENDdo                                                  *
012900120716     c*
013000120716     c                   eval      *in72 = *on
013100980525     c*
013200980525     c                   endsr
013300980525     C*-------------------------------------------------------------------------*
013400980525     c* SUB_GESBF Routine Gestione Subfile
013500980525     C*-------------------------------------------------------------------------*
013600980525     c     SUB_GESBF     BEGSR
013700980525     C*
013800971015     C* nessun record caricato
013900151020     c                   exsr      carsfl
014000980526    1c                   if        nrr2 = 0
014100151020     c                   eval      *in71 = *off
014200980526    xc                   else
014300151020     c                   eval      *in71 = *on
014400980526    1c                   endif
014500000000     C*
014600980526     C                   WRITE     bs02z01                                      *
014700980526     C                   WRITE     bs02t01                                      *
014800981030?   1C                   do        *hival                                       *
014900971015     c* sfl controlo
015000980526     C                   EXFMT     bs02c01                                      *
015100981030     c* F12= fine lavoro
015200120716    2C                   if        *inkl = *on
015300120716     c                   leavesr
015400120716    2c                   endif
015500120716     c*
015600120716?   1C                   enddo                                                  *do  hival
015700971021     c**
015800971016     c                   endsr
