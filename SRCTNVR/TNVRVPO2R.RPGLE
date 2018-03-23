000100090402     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200090402
000300090402      * -------------------------------------------------------------*
000400091125      * Crea file visite TIVIS solo filiale 001
000500090402      * -------------------------------------------------------------*
000600090402
000700090714     fTnofm01l  if   e           k Disk
000800090714     fTnvis05l  if   e           k Disk
000900090715     fTivis00f  uf a e             Disk
001000090715     fTivof00f  uf a e             Disk
001001091125     fTfntc00f  uf a e             Disk
001100090715
001200090715      * ---------------------------------------------------------
001300090715     d dsta01        e ds
001400090402
001401091125      // - Ricerca ultimo numero contatti
001402091125     d trul33ds      e ds                  inz
001403091125
001404091125     d $NumOK          s               n   inz(*off)
001500090715      * ---------------------------------------------------------
001600090715     iTnvis000
001700090715     i              visesi                      ex_visesi
001800090715
001900090715      * ---------------------------------------------------------
002000090402
002100090715     c     *loval        setll     tnvis05l
002200090715     c                   do        *hival
002300090715     c                   read      tnvis05l
002400090715     c                   if        %eof(tnvis05l)
002500090715     c                   leave
002600090715     c                   endif
002700090715      *
002701091125      * verifico se commerciale della filiale che parte
002702091125     c                   If        subst(digit(viscmm):1:3) <> '001'
002703091125     c                   iter
002704091125     c                   endif
002705091125
002800090715     c                   eval      viscmmi= viscmm
002900090715     c                   eval      visdat = visdvi
003000090715      * se visita chiusa imposto data chiusura con al data conferma visita + grande tra l'estera
003100090715      * e l'italia
003200090715
003300090715     c                   clear                   visdch
003400090715
003500090715     c                   if        visfsv = 'C' and visdcv > visdne
003600090715     c                   eval      visdch = visdcv
003700090715     c                   endif
003800090715
003900090715     c                   if        visfsv = 'C' and visdne > visdcv
004000090715     c                   eval      visdch = visdne
004100090715     c                   endif
004200090715
004300090715     c                   movel     viscmm        visfil
004400090715
004401091125      * recupero il nuovo numero
004402091125
004403091125     c                   reset
004404091125     c                   dow       $numok = *off
004405091125     c                   clear                   trul33ds
004406091125     c                   eval      i33tla = 'L'
004407091125     c                   eval      i33ope = 0
004408091125     c                   eval      i33Cnu = 080
004409091125     c                   eval      i33num = 1
004410091125     c                   movel     trul33ds      kpjbu
004411091125     c                   call      'TRUL33R'
004412091125     c                   parm                    kpjba
004413091125     c                   movel     kpjbu         trul33ds
004414091125
004415091125     c                   If        o33err = 0
004416091125     c                   eval      visnrv = o33nrf
004417091125     c     visnrv        setll     tivis05l
004418091125     c                   if
004500090715     c                   write     tivis000
004600090715      * verifico se esistono delle tariffe legate alla visita
004700090402
004800090715     c     visnrv        setll     tnofm01l
004900090402     c                   Do        *hival
005000090402      * Leggo il file
005100090715     c     visnrv        Reade     tnofm01l
005200090402      * Fine file esco
005300090715     c                   If        %Eof(Tnofm01l)
005400090402     c                   Leave
005500090402     c                   EndIf
005600090402
005700090715     c                   clear                   tivof000
005800090715      * valorizzo con i campi delle tariffe
005900090715     c                   eval      vofnrv = tamksc
006000090715     c                   eval      vofctr = tamctr
006100090715      * il progressivo non lo valorizzo in quanto è sempre a zero
006200090715      * il flag del tipo tariffa viene impostato in base ad italia e network in caso di estero
006300090715     c                   eval      voftpt = tamfie
006400090715
006500090715     c                   eval      dsta01 = tamflo
006600090715      * il flag del tipo tariffa viene impostato DPD se italia e network DPD
006700090715     c                   if        §tadpd =  'S'
006800090715     c                   eval      voftpt = 'D'
006900090715     c                   endif
007000090715      * il flag del tipo tariffa viene impostato FEDEX  se network FEDEX
007100090715     c                   if        §tafed =  'S'
007200090715     c                   eval      voftpt = 'F'
007300090715     c                   endif
007400090715      * l'esito non lo memorizzo in quanto se offerte confermate non esistono + nel file
007500090715
007600090715      * data presentazione offerta la prendo dalla testata in base al tipo tariffa italia e network
007700090715      * esteri
007800090715     c                   if        voftpt = 'I'
007900090715     c                   eval      vofdpo = visdpo
008000090715     c                   else
008100090715     c                   eval      vofdpo = visdoe
008200090715     c                   endif
008300090715
008400090715     c                   write     tivof000
008500090715
008600090715     c                   enddo
008700090715
008800090715     c                   enddo
008900090402
009000090402     c                   Eval      *InLr = *On
