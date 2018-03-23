000100041018     H*---------------------------------------------------------------*
000200160223     H* FNLVDPR  Verifica tabella IED Cper instradamento da EEX a DPD *
000300041018     H*---------------------------------------------------------------*
000400041018     H* INDICATORI USATI                                              *
000500041018     H*---------------------------------------------------------------*
000600041018     F*---------------------------------------------------------------*
000700041018     F* DATA BASE                                                     *
000800041018     F*---------------------------------------------------------------*
000900041018     H DECEDIT('0,') DATEDIT(*DMY.)
001000050418     F*
001100160223     Ftntbe01l  IF   E           K DISK
001200041018
001300160223     d kcod            s                   like(tbecod) inz('IED')
001400160223     d kke1            s                   like(tbeke1)
001500160223     d kke2            s                   like(tbeke2)
001600041018     D*------------------
001700041018     D* ARCHITETTURA
001800041018     D*------------------
001900041018     D KPJBA         E DS
002000041018     D*------------------
002100041018     D* PARAMETRI LANCIO
002200041018     D*------------------
002300041021     d*
002400160223     D Died          E DS
002500160223     D fnlvdpds      E DS
002600130621     d Azuteds       e ds                  Extname(Azute00f)
002700050419     d dDatiute      e ds
002800050419     d Tibs34ds      e ds                  Inz
002900041018     C*---------------------------------------------------------------*
003000041018     C* MAIN LINE                                                     *
003100041018     C*---------------------------------------------------------------*
003200130621     c     *entry        plist
003300130621     c                   parm                    kpjba
003400160223     c                   movel     kpjbu         fnlvdpds
003500130624
003600160223     c* pulizia campi di output
003700160223     c                   clear                   odpesito
003800160223     c                   clear                   odperr
003900160223     c                   clear                   odpmsg
004000160223
004100130624     C* TIPO LANCIO: "C" -           CHIUSO CON LR
004200130624     C* TIPO LANCIO: "L" - ELABORO E CHIUDO CON LR
004300130624     C* TIPO LANCIO: " " - ELABORO E CHIUDO IN RETRN
004400160223    0c                   if        idptla<>'C'
004500160223     c* Errore --> LNA OBBLIGATORIA
004600160223   1ac                   if        idplna=0
004700160223     c                   eval      odperr='E'
004800160223  x1ac                   else
004900160223     c* 1°Tentativo di ricerca: cerco cliente per tutte le naz/lna
005000160223     c                   movel(p)  999           kke1
005100160223     c                   movel(p)  999           kke2
005200160314     c                   eval      %subst(kke2:4:7)=%editc(idpccm:'X')
005300160223     c                   exsr      sr_chainTntbe
005400160223    1c                   if        odpesito=*blanks
005500160223     c* verifico se lna presente
005600160226     c                   movel(p)  idplna        kke1
005700160223     c     ktab1         setll     tntbe01l
005800160223    2c                   if        %equal(tntbe01l)
005900160223     c* Se presente lna:
006000160223     c* 1- cerco con chiave completa LNA/NAR/CCM
006100160223     c                   movel(p)  idplna        kke1
006200160223     c                   movel(p)  idpnaz        kke2
006300160223     c                   eval      %subst(kke2:4:7)=%editc(idpccm:'X')
006400160223     c                   exsr      sr_chainTntbe
006500160223    3c                   if        odpesito=*blanks
006600160223     c* 2- cerco con LNA/NAR
006700160223     c                   eval      %subst(kke2:4:7)=*blanks
006800160223     c                   exsr      sr_chainTntbe
006900160223    4c                   if        odpesito=*blanks
007000160223     c* 3- cerco con sola LNA
007100160223     c                   clear                   kke2
007200160223     c                   exsr      sr_chainTntbe
007300160223    4c                   endif
007400160223    3c                   endif
007500160223    2c                   endif
007600160223    1c                   endif
007700160223     c* se non trovati record --> OK abilitazione
007800160223     c                   if        odpesito=*blanks
007900160223     c                   movel     1             odpesito
008000160223     c                   endif
008100130624     c*
008200160223   1ac                   endif
008300160226     c                   movel     fnlvdpds      kpjbu
008400130624    0c                   endif
008500130624
008600160223    1C     idptla        IFEQ      ' '
008700130624     C                   RETURN
008800130624     C                   ELSE
008900041022     c                   seton                                        lr
009000130624     c                   endif
009100160223     c**********************************************************************
009200160223     c     sr_chainTntbe begsr
009300160223     c     ktab          chain     tntbe01l
009400160223    1c                   if        %found(tntbe01l)  and tbeatb=*blanks
009500160223     c                   movel     tbeuni        died
009600160223     c                   if        §iedabi='N'
009700160223     c                   movel     0             odpesito
009800160223     c                   else
009900160223     c                   movel     1             odpesito
010000160223     c                   endif
010100160223     c                   endif
010200160223     c                   endsr
010300041018     c**********************************************************************
010400041018     c     *inzsr        begsr
010500041022     c*
010600050419     c*
010700050419     c     *dtaara       define    §azute        azuteds
010800050419     c     *dtaara       define    §datiute      ddatiute
010900050419     c                   in(E)     *dtaara
011000050419     c                   If        %error  or RSUT = *blanks
011100050419     c                   CLEAR                   tibs34ds
011200050419     c                   CALL      'TIBS34R'
011300050419     c                   PARM                    tibs34ds
011400050419     c                   in        *dtaara
011500050419     c                   EndIf
011600041018     c
011700160223     c     ktab          klist
011800160223     c                   kfld                    KCOD
011900160223     c                   kfld                    KKE1
012000160223     c                   kfld                    KKE2
012100160223     c     ktab1         klist
012200160223     c                   kfld                    KCOD
012300160223     c                   kfld                    KKE1
012400050418     c                   endsr
