000100040315     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040316
000300040316      *------------------------------------------------------------------------*
000400040316      *                                                                        *
000500100412      *   STACCA NUMERO PER NUOVA VISITA/TRATTATIVA                            *
000600040316      *                                                                        *
000700040316      *------------------------------------------------------------------------*
000800040512
001000100412     fTivis05l  if   e           k Disk
001100100623     fTfaco00f  if   e           k Disk
001200040316
001300100413      *------------------------------------------------------------------------*
001400100413      *   S C H I E R E
001500100413      *------------------------------------------------------------------------*
001600040316
001700100413      *------------------------------------------------------------------------*
001800100413      *   V A R I A B I L I
001900100413      *------------------------------------------------------------------------*
002000100413
002100100413     d Newnrv          s                   like(visnrv)
002200100413     d $prg            s              2  0
002300100413
002400100413      // - Flags booleani
002500100413     d $NumOK          s               n   inz(*off)
002600040316
002700100413       //----------------------------------------------------------------------*
002800100413       // D S   I N T E R N E / E S T E R N E
002900100413       //-----------------------------------------------------------------------*
003000040512     d Kpjba         e ds
003100100412     d Trul48ds      e ds
003200100413     d Trul33ds      e ds                  inz
003300040316
003400100413       //----------------------------------------------------------------------*
003500100413       // C O S T A N T I
003600100413       //----------------------------------------------------------------------*
003700040316
003800100413       //----------------------------------------------------------------------*
003900100413       //RIEPILOGO INDICATORI
004000100413       //----------------------------------------------------------------------*
004100100413       // -
004200100413       //---------------------------------------------------------------------*
004300100413
004400100413       //--------------------------------------------------------------
004500100413       //?prototipi
004600100413       //---------------------------------------------------------------
004700100413
004800100413
004900100413     D Trul33R         pr                  extpgm('TRUL33R')
005000100413     D  Kpjba                              likeds(kpjba) const
005100100413
005200100413        //--------------------------------------------------------------------*
005300040316
005400100413     c     *Entry        plist
005500100413     c                   parm                    KPJBA
005600100413     c                   parm                    trul48ds
005700100413
005800100413      /free
005900100413
006000100413            reset $NumOk ;
006100100413
006200100413            dow $NumOk = *off ;
006300040512
006400100412                RESET trul33ds;
006500100412                i33tla = 'L'  ;
006600100412                i33ope = 0    ;
006700100413                i33Cnu = 1    ;
006800100412                i33num = 1    ;
006900100412                kpjbu = trul33ds;
007000100412                trul33r(kpjba);
007100100412                trul33ds = kpjbu;
007200100412
007300100413                If o33Err = 0;
007400100412                   newnrv  = o33Nrf;
009000100413       // verifico se numero gi� esistente  prima su TIVIS
009100100413                   setll newnrv tivis05l ;
009200101021                   IF  not %equal(tivis05l)  ;
010100100623       // se esiste su TFaco stacco un nuovo numero
010200100412
010300101021                      acokut = 1 ;
010400101021                      acokcc = 151 ;
010500101021                      acoksc = o33nrf ;
010600101021                      chain(n) (acokut:acokcc:acoksc) Tfaco00f ;
010700101021                      If %found(Tfaco00f) ;
010800101021                         iter ;
010900101021                      Endif ;
011000100623
011100101021                      $NumOk = *on ;
011200101021                      o48nrv = o33nrf ;
011201101021                   else ;
011202101021                      iter ;
011203101021                   endif ;
011300101021                else ;
011400100413                   o48err = o33err ;
011500100413                   o48msg = o33msg ;
011600100413                   leave;
011700100412                Endif;
011800100412        // se esiste gi� questo numero richiamo trul33
011900100412            Enddo;
012000040316
012100100413            Eval  *InLr = *On ;
012200040318
