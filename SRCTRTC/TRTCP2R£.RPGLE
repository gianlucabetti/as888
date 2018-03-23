000100080423     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200140206     H*--------------------------------------------------------------*
000300140206     H*
000400140206     H* crea file spia per invio sms/mail ai destinatari in seguito a l.avv
000500080423     H*
000600000000     H*--------------------------------------------------------------*
000700080624     Ffidst01l  if   e           k disk
000800140206     Ffnarb01l  if   e           k disk
000900140508     Ftabel00f  if   e           k disk
001000140206     Ffisia01l  if   e           k disk    rename(fisia000:sia001) infds(fisia)
001100140206     FFIsia00F  uf a E             DISK
001200140206     d fisia           ds
001300140206     d  rrnsia               397    400b 0
001400140207     d fisiads       e ds                  extname(fisia00f)
001500140207     d dsiaric       e ds
001600140508     d dsiaflo       e ds
001700140206     d fnlv81ds      e ds
001800140212     d fnlv82ds      e ds
001900140206     d trtcp2ds      e ds
002000080624     d ddstflr       e ds
002100140508     d ds7r          e ds
002200080624     d cat4            s              1  0 inz(4)
002300140212     d tipa            s              3
002400140213     d dataiso         s               d   datfmt(*iso)
002500140213     D dateu           S              8  0
002600080423     d
002700140213     c                   exsr      routinz
002800140206     c* il pgm può essere chiamato in inserimento o in cancellazione
002900140206     c                   if        t2chiamata <> 'A'
003000140206     c                   exsr      scrivo
003100140206     c                   else
003200140206     c                   exsr      cancello
003300140206     c                   end
003400140206     c                   seton                                        lr
003500140206     c**********************************************************************
003600140206     c     scrivo        begsr
003700140206     c**********************************************************************
003800140212     c* Solo per Lasciato Avviso:
003900140212    1c                   if        t2chi<>'F'
004000140206      * se fase di test esco senza fare nulla
004100080624     c                   clear                   ddstflr
004200080624     c     kdst          chain     fidst01l
004300140206     c                   if        %found(fidst01l)
004400080624     c                   movel     dstflr        ddstflr
004500080624     c                   endif
004600140214     c                   if        §DSTTSTPDA <> *blank and t2chi = 'P'
004700080624     c                   return
004800080624     c                   endif
004900140509    1c                   endif
005000140206     C* la spedizione deve essere al primo tentativo di consegna
005100140206     C     karb          chain     Fnarb01l
005200140512     c                   if        not %found(fnarb01l)
005300140512     c                   return
005400140512     c                   endif
005500140206     c                   if        %found(fnarb01l) and arbntc = 0
005600140206     c                   else
005700140509     c                   if        %found(fnarb01l) and
005800140509     c                             t2chi <> 'F' and arbntc = 0
005900140206     c                   return
006000080423     c                   endif
006100140509    1c                   endif
006200140508     C* verifico se la chiamata è per fermo deposito e la particolarità è valorizzata
006300140508     c                   clear                   ds7r
006400140508     c                   if        arbgma <> *blank and t2chi = 'F'
006500140508     c                   movel(p)  arbgma        tblkey
006600140508     c     ktab          chain     tabel00f
006700140508     c                   if        %found(tabel00f)
006800140508     c                   movel     tbluni        ds7r
006900140508     c                   endif
007000140508     c                   endif
007100140206     C* verifico se ho l'indirizzo mail o il telefono del destinatario
007200140206     c                   clear                   fnlv81ds
007300140206     c                   eval      ILV81AAS = t2aas
007400140206     c                   eval      ILV81lnp = t2lnp
007500140206     c                   eval      ILV81nrs = t2nrs
007600140206     c                   eval      ILV81nsp = t2nsp
007700140206     c                   eval      ILV81trc = 'A'
007800140206     c                   call      'FNLV81R'
007900140206     c                   parm                    fnlv81ds
008000140206     c                   if        OLV81EML = ' ' and OLV81tel = ' '
008100140206     c                   return
008200140206     c                   end
008300140206     c* controllo se è già presente la spia
008400140212     c                   clear                   fnlv82ds
008500140212     c                   eval      ILV82AAS = t2aas
008600140212     c                   eval      ILV82lnp = t2lnp
008700140212     c                   eval      ILV82nrs = t2nrs
008800140212     c                   eval      ILV82nsp = t2nsp
008900140212     c                   eval      ILV82tipa= tipa
009000140212     c                   call      'FNLV82R'
009100140212     c                   parm                    fnlv82ds
009200140212     c                   if        OLV82STS <>'9'
009300140206     c                   return
009400140206     c                   end
009500140206     c* scrivo la spia
009600140206     c                   clear                   rrnsia
009700140206     c     ksia          chain     fisia01l
009800140206     c* se il record è già presente da elaborare o elaborato esco dal pgm
009900140206     c                   if        %found(fisia01l) and
010000140206     c                             (siasts = '0' or
010100140206     c                             siasts = '1')
010200140206     c                   return
010300140206     c                   end
010400140206     c* se il record è già presente ma non ha avuto esito corretto tento
010500140206     c* di aggiornarlo
010600140206     c                   if        %found(fisia01l)
010700140206     c     rrnsia        chain(e)  FISIA00f
010800140206     c                   if        %error
010900140206     c                   return
011000140206     c                   end
011100140512     c                   eval      dsiaflo = siaflo
011200140512     c                   else
011300140512     c                   clear                   dsiaflo
011400140206     c                   end
011500140508      * salvo flag se esiste rekord
011600140508      *
011700140206     c                   clear                   FISIAds
011800140206     c                   time                    SIADAORI
011900140206     c                   eval      SIAAAS = t2aas
012000140206     c                   eval      SIALNP = t2lnp
012100140206     c                   eval      SIANRS = t2nrs
012200140206     c                   eval      SIANSP = t2nsp
012300140206     c                   eval      SIATIPA= tipa
012400140206     c                   eval      SIASTS ='0'
012500140212     c* Solo per Lasciato Avviso:
012600140212    1c                   if        t2chi<>'F'
012700140206     c* il pgm può essere richiamato sia in ricezione record CET se da PDA
012800140206     c* oppure da chiusura distinta
012900140207     c                   clear                   dsiaric
013000140207     c                   eval      §siachi = t2chi
013100140207     c                   eval      §siadev = t2dev
013200140207     c                   eval      §siahev = t2hev
013300140207     c                   eval      sianote= dsiaric
013400140508      * per fermo deposito
013500140508     c                   else
013600140508     c                   eval      §siafd = §7rfd
013700140508     c                   eval      siaflo = dsiaflo
013800140212    1c                   endif
013900140206     c* se l'ho trovato ma è "anomalo" provo a rimandarlo
014000140206     c                   if        %found(fisia00f)
014100140206     c                   update    FISIA000                             01
014200140206     c                   else
014300140206     c                   write     FISIA000                             01
014400140206     c                   end
014500140206     c*
014600140206     C                   endsr
014700140206     c**********************************************************************
014800140206     c     cancello      begsr
014900140206     c**********************************************************************
015000140206     c* cancello la spia se non ancora elaborata
015100140206     c     ksia          chain     fisia01l
015200140206     c                   if        %found(fisia01l) and
015300140206     c                             siasts = '0'
015400140206     c     rrnsia        chain(e)  FISIA00f
015500140206     c                   if        %error
015600140206     c                   return
015700140206     c                   else
015800140206     c                   delete    FISIA000                             01
015900140206     c                   end
016000140206     c                   end
016100140206     c*
016200140206     C                   endsr
016300140213     c**********************************************************************
016400140213     c     routinz       begsr
016500140213     c**********************************************************************
016600140213     c* Imposto tipo alert a seconda della chiamata F=Fermo deposito  altrimenti RIC
016700140213     c                   if        t2chi<>'F'
016800140213     c                   eval      tipa='RIC'
016900140213     c                   else
017000140213     c                   eval      tipa='FD '
017100140213     c                   endif
017200140213     c                   endsr
017300140206     c**********************************************************************
017400140206     c     *inzsr        begsr
017500140206     c**********************************************************************
017600140206     c     *entry        plist
017700140206     c                   parm                    trtcp2ds
017800140212     c*
017900140206     c*
018000140206     c     kdst          klist
018100140206     c                   kfld                    cat4
018200140206     c                   kfld                    t2ndc
018300140206     c                   kfld                    t2fgs
018400140206     c*
018500140206     c     karb          klist
018600140206     c                   kfld                    t2aas
018700140206     c                   kfld                    t2lnp
018800140206     c                   kfld                    t2nrs
018900140206     c                   kfld                    t2nsp
019000140206     c*
019100140206     c     ksia          klist
019200140206     c                   kfld                    t2aas
019300140206     c                   kfld                    t2lnp
019400140206     c                   kfld                    t2nrs
019500140206     c                   kfld                    t2nsp
019600140206     c                   kfld                    TIPA
019700140508     c*
019800140508     c     ktab          klist
019900140508     c                   kfld                    tblkut
020000140508     c                   kfld                    tblcod
020100140508     c                   kfld                    tblkey
020200140508     c                   z-add     1             tblkut
020300140508     c                   movel     '7R'          tblcod
020400140206     c                   endsr
