000100121218      //---------------------------------------------------------------
000200121218      //
000300121219      //?      Sistema flag FLOIML per bolle presenti su TILAC
000400121218      //
000500121218      //---------------------------------------------------------------
000600121218
000700121218     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000121218
001100121219     fTILAC01L  if   e           k disk
001200121218     fTITAS30C  uf   e           k disk
001300040119
001400040119      *---------------------------------------------------------------*
001500050330
001600121218     d dTASflo       e ds                  inz
001700121218
001800121218      // Parametri
001900121218     d kpjba         e ds
002000121218
002100121218       // - flag booleani
002200121218     d wEoF            s               n
002300121219     d wmonica         s               n
002400121219
002500121219       // - Campi di comodo
002600121219     d wPos1           s              2  0 inz
002700090212
002800090212       //--------------------------------------------------------------
002900090212       //?Definizione procedure usate.                                 ?
003000090212       //--------------------------------------------------------------
003100040119
003200040119      *---------------------------------------------------------------*
003300040119
003400040119      * Predisposizione indicatori per riconoscere i tipi record
003500040119      *   reperiti dal file TITAS30C
003600121219     iTITAS000      31
003700121219     iTITAS010      32
003800121219     iTITASP00      33
003900040119
004000040119      *---------------------------------------------------------------*
004100121218
004200121218     c     *entry        plist
004300121218     c                   parm                    kpjba
004400121218
004500121219     c     KTAS30        klist
004600121218     c                   kfld                    TASaas
004700121218     c                   kfld                    TASlnp
004800121218     c                   kfld                    TASnrs
004900121218     c                   kfld                    TASnsp
005000040119
005100121218      /free
005200121218
005300121218        wEoF = *off;
005400121218
005500121219       // Leggo TILAC
005600121219        LACaas = '12';
005700121219        setll (LACaas) TILAC01L;
005800121218
005900121218        DOW not wEoF;
006000121219          reade (LACaas) TILAC01L;
006100121218
006200121218       // fine file
006300121219          IF  %eof(TILAC01L);
006400121218            leave;
006500121218          ENDIF;
006600121219
006700121219          IF LACtela <> 'A';
006800121219            iter;
006900121219          ENDIF;
007000121219
007100121219          wPos1 = %scan('  ':LACdir);
007200121219          wPos1 -= 3;
007300121219          IF  %subst(LACdir:wPos1:3) = '/PF';
007400121219            iter;
007500121219          ENDIF;
007600121219
007700121219          wmonica = *off;
007800121219
007900121219       // Aggancio la bolla
008000121219          *in31 = *off;
008100121219          *in32 = *off;
008200121219          *in33 = *off;
008300121219
008400121219          TASaas = 2012;
008500121219          TASlnp = %dec(LAClnp:3:0);
008600121219          TASnrs = %dec(LACnrs:2:0);
008700121219          TASnsp = %dec(LACnsp:7:0);
008800121219          setll (TASaas:TASlnp:TASnrs:TASnsp) TITAS30C;
008900121219          reade (TASaas:TASlnp:TASnrs:TASnsp) TITAS30C;
009000121219          DOW  not %eof(TITAS30C);
009100121218
009200121219         // solo le bolle con flag immagine = 'S'
009300121219            dTASflo = TASflo;
009400121219            IF  �FLOiml <> 'S';
009500121219              leave;
009600121219            ENDIF;
009700121219
009800121219            �FLOiml = 'I';
009900121219            TASflo = dTASflo;
010000121219
010100121219            IF  wmonica;
010200121219              leave;
010300121219            ENDIF;
010400121219
010500121219            IF  *in31;
010600121219              UPDATE  TITAS000;
010700121219            ENDIF;
010800121219            IF  *in32;
010900121219              UPDATE  TITAS010;
011000121219            ENDIF;
011100121219            IF  *in33;
011200121219              UPDATE  TITASP00;
011300121219            ENDIF;
011400121219
011500121219            *in31 = *off;
011600121219            *in32 = *off;
011700121219            *in33 = *off;
011800121219            reade (TASaas:TASlnp:TASnrs:TASnsp) TITAS30C;
011900121219          ENDDO;
012000121219
012100121219        ENDDO;
012200121219
012300121219        *inlr = *on;
