000100170504     H DECEDIT('0,') DATEDIT(*YMD.)
000200170505     ffiarbt0f  if   e             disk
000202170510     ffnfta01l  if a e           k disk
000203170510     ffnfta00f  o    e           k disk    usropn
000204170510     f                                     rename(fnfta000:fnfta0)
000205170622     ftitas40c  if   e           k disk    ignore(titasp00)
000207170510     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000208170510     d yy              s              6  0 inz
000209170510     d ix              s              3  0 inz
000210170510     d cont            s              6  0 inz
000211170510     d wfnfta          s              1    inz
000212170622     d wdpart          s              1    inz
000213170510
000214170510     C     *entry        plist
000215170510     C                   parm                    aggio             1
000216170504
000217170510       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
000218170510
000219170510        open fnfta00f;
000220170510
000221170508        read fiarbt0f ;
000222170508        dow not %eof(fiarbt0f);
000300170508           if ARBDFT>=20170101 and (arbcvb ='T6' or arbcvb='26');
000301170622             setll (arbFIV:arbnft:arbdft) titas40c ;
000302170622             if not %equal(titas40c);
000303170510               setll (arbFIV:arbdft:arbnft) fnfta01l ;
000304170508               if not %equal(fnfta01l);
000305170510                if aggio='S'   ;
000306170508                  exsr sr_fnfta;
000307170510                endif;
000308170508               endif;
000309170622             endif;
000400170508           endif;
000401170508           read fiarbt0f;
000500170504        enddo;
000501170510
000502170510        close fnfta00f ;
000503170510
000504170510        for ix=1 to 450;
000505170510          cont = 1;
000506170510          setll ix titas40c;
000507170510          reade ix titas40c;
000508170510          dow not %eof(titas40c);
000509170510             if tasdft >=20170101 and tasdft<=20171231 ;
000510170510               select;
000511170510               when cont<tasnft;
000512170510                  for yy=cont to (tasnft-1)  ;
000513170510                         clear wfnfta  ;
000514170510     c                   eval       SQLCOD = 0
000515170510     C/EXEC SQL
000516170510     C+ SELECT  'S' into :wfnfta
000517170510     C+   FROM  fnfta00f   WHERE  ftafiv = :tasfiv and
000518170605     c+                           ftanft = :yy
000519170510     C+    fetch first row only
000520170510     C/END-EXEC
000521170510                     if sqlcod<>0 or wfnfta<>'S';
000522170622       // verifico che non sia relativa ad una bolla ancora da partire
000523170622                         clear wdpart  ;
000525170622     c                   eval       SQLCOD = 0
000526170622     C/EXEC SQL
000527170622     C+ SELECT  'S' into :wdpart
000528170622     C+   FROM  filtra201/fiar600f WHERE  ar6fiv = :tasfiv and
000529170622     c+                           ar6nft = :yy
000530170622     C+    fetch first row only
000531170622     C/END-EXEC
000532170622
000534170510       //      La fattura mancante non è su fnfta --> errore
000535170622                     if wdpart=*blanks;
000536170510                         except erro;
000537170622                     endif;
000538170510                     endif;
000539170510                  endfor;
000540170510                  cont=tasnft ;
000541170510                  exsr ela_storno;
000542170510
000543170510               when cont=tasnft;
000544170510                 exsr ela_storno;
000550170510               when cont>tasnft;
000551170510                      except errx ;
000552170510               endsl;
000553170510               cont+=1;
000554170510             endif;
000555170510             reade ix titas40c;
000556170510          enddo;
000557170508
000558170510        endfor;
000559170510
000560170508        *inlr = *on ;
000600170508        //-----------------------------------------------------------------------*
000700170508        // Memorizzazione dati fattura annullata/stornata
000800170508        //-----------------------------------------------------------------------*
000900170508        begsr sr_Fnfta     ;
000901170510        clear  fnfta0      ;
001000170508        FTAAAS = arbaas    ;
001100170508        FTALNP = arblnp    ;
001200170508        FTANRS = arbnrs    ;
001300170508        FTANSP = arbnsp    ;
001400170508        //FTAMGS = arbmgs    ;
001500170508        FTADIV = arbdiv    ;
001600170508        FTAALI = arbali    ;
001700170508        FTACEI = arbcei    ;
001800170508        FTAIFT = arbift    ;
001900170508        FTAFIV = arbfiv    ;
002000170508        FTANFT = arbnft    ;
002100170508        FTADFT = arbdft    ;
002200170508
002300170510        write(e) fnfta0      ;
002400170508        endsr;
002500170510        //-----------------------------------------------------------------------*
002600170510        // Memorizzazione dati fattura annullata/stornata
002700170510        //-----------------------------------------------------------------------*
002800170510        begsr sr_Fnfta_tas ;
002900170510        clear  fnfta000    ;
003000170510        FTAAAS = tasaas    ;
003100170510        FTALNP = taslnp    ;
003200170510        FTANRS = tasnrs    ;
003300170510        FTANSP = tasnsp    ;
003400170510        //FTAMGS = arbmgs    ;
003500170510        FTADIV = tasdiv    ;
003600170510        //AALI = tasali    ;
003700170510        FTACEI = tasfei    ;
003800170510        //AIFT = tasift    ;
003900170510        FTAFIV = tasfiv    ;
004000170510        FTANFT = tasnft    ;
004100170510        FTADFT = tasdft    ;
004200170510
004300170510        write(e) fnfta000    ;
004400170510        endsr;
004401170510        //-----------------------------------------------------------------------*
004402170510        //-----------------------------------------------------------------------*
004403170510        begsr ela_storno;
004404170510                 if tastbl = 'AP' or tastbl='AS';
004405170510       // se storno verifico se già memorizzato
004406170510                      setll (tasfiv:tasdft:tasnft) fnfta01l   ;
004407170510                      if not %equal(fnfta01l);
004408170510       //          memorizzo
004409170510                        if aggio='S';
004410170510                         exsr sr_fnfta_tas;
004411170510                        endif;
004412170510                      endif;
004413170510                 endif  ;
004414170510        endsr;
004500170510     OQSYSPRT   E            erro       01
004600170510     O                                         +  0 'Manca fattura:'
004601170510     o                                         +  1 'Fil.IVA'
004700170510     O                       ix            z   +  1
004701170510     o                                         +  1 'N.ro'
005100170510     O                       yy            z   +  1
005700170510     O          E            errx       01
005800170510     o                       tasfiv        z   +  1
005900170510     o                       cont          z   +  1
006000170510     o                       tasnft        z   +  1
