000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200140919     H* FNLRY09R *---------------------------------------------------*
000300140919     H* - Driver di controllo presenza / esecuzione indicazioni di cons.
000400110610     h*
000500000000     H*--------------------------------------------------------------*
000600140919     FTIIDC05l  IF   E           K DISK
000700140929     FTIIDC01l  IF   E           K DISK    rename(tiidc000:tiidc001)
000800030722     F**
000900070313     d
001000140919     D fnlry09ds     E DS
001100140919     D fnlry03ds     E DS
001200140930     D fnlry02ds     E DS
001300140919     D kpjba         E DS
001400070313     d
001500140929     d WELADATA        s                   like(IDCELADATA) INZ(0)
001600140206
001700900518     C***************************************************************
001800000000     C     *ENTRY        PLIST
001900140919     C                   PARM                    fnlry09ds
002000940223     C*---------------------------------------------------------------*
002100140922     C* TIPO LANCIO, ilry09TLA "C" -           CHIUSO CON LR
002200140922     C* TIPO LANCIO, ilry09TLA "L" - ELABORO E CHIUDO CON LR
002300140922     C* TIPO LANCIO, ilry09TLA " " - ELABORO E CHIUDO IN RETRN
002400941216     C*
002500140919    1C     ilry09tla     IFNE      'C'
002600140919     c                   clear                   OLry09esit
002700140919     c                   clear                   OLry09err
002800140919     c                   clear                   OLry09msg
002900070412     c
003000051123     c
003100140919     c* controlli formali dei parametri ricevuti
003200140919     c                   exsr      sr_chkform
003300140919
003400140919     c                   if        olry09err=' '
003500140919     c                   select
003600140919     c* Richiesto check
003700140922     c                   when      ilry09tch='C'
003800140919     c                   exsr      sr_Check
003900140919     c* Richiesta esecuzione indicazioni
004000140922     c                   when      ilry09tch='E'
004100140919     c                   exsr      sr_esegui
004101141001     c                   exsr      sr_Check
004200140919     c                   endsl
004300140919     c*
004400140205    1c                   endif
004500140919    1c                   endif
004600051123     c
004700140922    1C     ilry09tla     IFNE      ' '
004800051123     C                   SETON                                        LR
004900051123     C                   ELSE
005000051123     C                   SETON                                        RT
005100140205    1C                   ENDIF
005200140206     C**************************************************************************
005300140919     C     sr_chkform    BEGSR
005400140919     c* tipo chiamata obbligatorio "C" o "E"
005500140919     c                   if        ilry09tch<> 'C' and
005600140919     c                             ilry09tch<> 'E'
005700140919     c                   eval      olry09err='1'
005800140919     c                   eval      olry09msg='Errore tipo chiamata'
005900140922     c                   leavesr
006000140919     c                   endif
006100140929     c* Filiale gestione obbligatoria se non passato il numero di spedizione
006200140929     c                   if        ilry09fgs=0 and ilry09nsp=0
006300140919     c                   eval      olry09err='2'
006400140930     c                   eval      olry09msg='Filiale gestione o numero spedi+
006500140930     c                             zione obbligatorio'
006600140922     c                   leavesr
006700140919     c                   endif
006800140206     c                   endsr
006900140206     C**************************************************************************
007000140919     C     sr_check      BEGSR
007100140919     c                   if        ilry09nsp>0
007200140929     c                   eval      olry09esit='0'
007300140929     c     kidc1         setll     tiidc01l
007400140929     c     kidc1         reade     tiidc01l
007500140929     c                   dow       not %eof(tiidc01l) and olry09esit='0'
007600140929     c                   if        idcelaflg='E'and
007700140929     c                             (ilry09fgs=0 or ilry09fgs=idcfgs)
007800140929     c                   eval      olry09esit='1'
007900140929     c                   endif
008000140929     c     kidc1         reade     tiidc01l
008100140929     c                   enddo
008200140919     c                   else
008300140919     c     ilry09fgs     setll     tiidc05l
008400140929     c                   if        %equal(tiidc05l)
008500140929     c                   eval      olry09esit='1'
008600140929     c                   else
008700140929     c                   eval      olry09esit='0'
008800140929     c                   endif
008900140919     c                   endif
009000140206     c                   endsr
009100140919     C**************************************************************************
009200140919     C     sr_esegui     BEGSR
009300140919     c                   if        ilry09nsp>0
009400140929     c     kidc1         setll     tiidc01l
009500140929     c     kidc1         reade     tiidc01l
009600140929     c                   dow       not %eof(tiidc01l)
009700140919     c
009800140929     C                   IF        IDCELAFLG='E' and
009900140929     c                             (ilry09fgs=0 or ilry09fgs=idcfgs)
010000140919     c                   clear                   fnlry03ds
010100140919     c                   eval      ilry03aas=idcaas
010200140919     c                   eval      ilry03lnp=idclnp
010300140919     c                   eval      ilry03nrs=idcnrs
010400140919     c                   eval      ilry03nsp=idcnsp
010500140919     c                   eval      ilry03prg=idcprg
010600140919     c                   eval      ilry03tipd=idctipodis
010700140919     c                   call      'FNLRY03R'
010800140919     c                   parm                    kpjba
010900140919     c                   parm                    fnlry03ds
011000140929     c                   endif
011100140929
011200140929     c     kidc1         reade     tiidc01l
011300140919     c                   enddo
011400140919     c                   else
011500140930     c                   clear                   fnlry02ds
011600140930     c                   eval      ILRY02SFL='S'
011700140930     c                   eval      ILRY02fgs=%editc(ILRY09FGS:'X')
011800140930     c                   movel(P)  fnlry02ds     kpjbu
011900140919     c                   call      'FNLRY02R'
012000140919     c                   parm                    kpjba
012100140919     c                   endif
012200140919     c                   endsr
012300991206     C**************************************************************************
012400051123     C     *INZSR        BEGSR
012500051123     c
012600140919     c     kidc5         Klist
012700140919     c                   Kfld                    ilry09fgs
012800140919     c                   Kfld                    ilry09aas
012900140919     c                   Kfld                    ilry09lnp
013000140919     c                   Kfld                    ilry09nrs
013100140919     c                   Kfld                    ilry09nsp
013200140929     c     kidc1         Klist
013300140929     c                   Kfld                    ilry09aas
013400140929     c                   Kfld                    ilry09lnp
013500140929     c                   Kfld                    ilry09nrs
013600140929     c                   Kfld                    ilry09nsp
013700140929     c                   Kfld                    WELADATA
013800140206     c
013900070313     c                   endsr
