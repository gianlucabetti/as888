000100121218      //---------------------------------------------------------------
000200121218      //
000300140407      //?        Creo TILAC da TILAC con nome immagine errato
000400121218      //
000500121218      //---------------------------------------------------------------
000600121218
000700121218     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000121218
001100140407     fTILAC_ERR if   e           k disk    rename(TILAC:ERRATO)
001200140407     f                                     prefix(e)
001300121218     fTILAC00F  o    e           k disk
001400140331     fTITAS30C  if   e           k disk
001500040119
001600040119      *---------------------------------------------------------------*
001700121218
001800121218      // Parametri
001900121218     d kpjba         e ds
002000121218
002100121218       // - Campi di comodo
002200140407     d k_aas           s                   like(TASaas)
002300140407     d k_lnp           s                   like(TASlnp)
002400140407     d k_nrs           s                   like(TASnrs)
002500140407     d k_nsp           s                   like(TASnsp)
002600121218
002700121218       // - flag booleani
002800121218     d wEoF            s               n
002900090212
003000090212       //--------------------------------------------------------------
003100090212       //?Definizione procedure usate.                                 ?
003200090212       //--------------------------------------------------------------
003300040119
003400040119      *---------------------------------------------------------------*
003500121218
003600121218     c     *entry        plist
003700121218     c                   parm                    kpjba
003800040119
003900121218      /free
004000121218
004100121218        wEoF = *off;
004200121218
004300140407       // Leggo file
004400121218        DOW not wEoF;
004500140407          read  TILAC_ERR;
004600121218
004700121218       // fine file
004800140407          IF  %eof(TILAC_ERR);
004900121218            leave;
005000121218          ENDIF;
005100140407          IF  elacela <> '33';
005200140407            k_aas = 2013;
005300140407            k_lnp = %dec(elaclnp:3:0);
005400140407            k_nrs = %dec(elacnrs:2:0);
005500140407            k_nsp = %dec(elacnsp:7:0);
005600140331       // Controllo se esiste la bolla
005700140407            chain (k_aas:k_lnp:k_nrs:k_nsp) TITAS30C;
005800140407            IF  not %found(TITAS30C);
005900140407              exsr elabora;
006000140407            ENDIF;
006100140407          ENDIF;
006200140331
006300121218        ENDDO;
006400121218
006500121218        *inLR = *ON;
006600121218
006700121218       //-------------------------------------------------------------**
006800121218       // Elabora
006900121218       //-------------------------------------------------------------**
007000121218        BEGSR elabora;
007100121218
007200121218          clear TILAC000;
007300140407          LACtim = elactim;
007400140407          LACaas = elacaas;
007500140407          LAClnp = elaclnp;
007600140407          LACnrs = elacnrs;
007700140407          LACnsp = elacnsp;
007800140407          LACxx1 = elacxx1;
007900140407          LACcop = elaccop;
008000140407          LACdir = 'ITALIAN IND';
008100140407          LACtpi = elactpi;
008200140407          LACnim = elacnim;
008300140407          LACidl = elacidl;
008400140407          LACela = '00';
008500140407          LACtela = elactela;
008600140407          LACksc = elacksc;
008700140407          LACksu = elacksu;
008800140407          LACtad = elactad;
008900140407          LACtadu = elactadu;
009000140407          LACaddms = elacaddms;
009100140407          LACfimp = elacfimp;
009200140407          LACimp = elacimp;
009300140407          LACidjob = elacidjob;
009400140407          LACutecd = elacutecd;
009500140407          LACtimecd = elactimecd;
009600121218
009700121218          WRITE  TILAC000;
009800140331
009900140331        ENDSR;
010000050330
010100121218      /end-free
010200140331
