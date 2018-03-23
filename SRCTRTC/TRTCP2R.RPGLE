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
001600140206     d fnlv81ds      e ds
001700140212     d fnlv82ds      e ds
001800140206     d trtcp2ds      e ds
001900080624     d ddstflr       e ds
002000140508     d ds7r          e ds
002100080624     d cat4            s              1  0 inz(4)
002200140212     d tipa            s              3
002300140213     d dataiso         s               d   datfmt(*iso)
002400140213     D dateu           S              8  0
002500080423     d
002600140213     c                   exsr      routinz
002700140206     c* il pgm può essere chiamato in inserimento o in cancellazione
002800140206     c                   if        t2chiamata <> 'A'
002900140206     c                   exsr      scrivo
003000140206     c                   else
003100140206     c                   exsr      cancello
003200140206     c                   end
003300140206     c                   seton                                        lr
003400140206     c**********************************************************************
003500140206     c     scrivo        begsr
003600140206     c**********************************************************************
003700140513     c*
003800140212     c* Solo per Lasciato Avviso:
003900140212    1c                   if        t2chi<>'F'
004000140513      * se distinta in test esco senza fare nulla
004100080624     c                   clear                   ddstflr
004200080624     c     kdst          chain     fidst01l
004300140206     c                   if        %found(fidst01l)
004400080624     c                   movel     dstflr        ddstflr
004500080624     c                   endif
004600140214     c                   if        §DSTTSTPDA <> *blank and t2chi = 'P'
004700080624     c                   return
004800080624     c                   endif
004900140513     c* verifico bolla
005000140206     C     karb          chain     Fnarb01l
005100140512     c                   if        not %found(fnarb01l)
005200140512     c                   return
005300140513     c                   end
005400140513     C* la spedizione deve essere al primo tentativo di consegna (RIC)
005500140513     c                   if        %found(fnarb01l) and arbntc <> 0
005600140206     c                   return
005700140509    1c                   endif
005800140513     C* verifico se la particolarità è con fd autorizzato dal mittente      lorizzata
005900140513     c* per memorizzarlo nel fisia in modo da inviare un messaggio
006000140513     c* nella mail o sms
006100140513     c                   clear                   ds7r
006200140513     c                   if        arbgma <> *blank
006300140508     c                   movel(p)  arbgma        tblkey
006400140508     c     ktab          chain     tabel00f
006500140508     c                   if        %found(tabel00f)
006600140508     c                   movel     tbluni        ds7r
006700140508     c                   endif
006800140508     c                   endif
006900140513    1c                   endif
007000140206     C* verifico se ho l'indirizzo mail o il telefono del destinatario
007100140206     c                   clear                   fnlv81ds
007200140206     c                   eval      ILV81AAS = t2aas
007300140206     c                   eval      ILV81lnp = t2lnp
007400140206     c                   eval      ILV81nrs = t2nrs
007500140206     c                   eval      ILV81nsp = t2nsp
007600140206     c                   eval      ILV81trc = 'A'
007700140206     c                   call      'FNLV81R'
007800140206     c                   parm                    fnlv81ds
007900140206     c                   if        OLV81EML = ' ' and OLV81tel = ' '
008000140206     c                   return
008100140206     c                   end
008200140206     c* controllo se è già presente la spia
008300140212     c                   clear                   fnlv82ds
008400140212     c                   eval      ILV82AAS = t2aas
008500140212     c                   eval      ILV82lnp = t2lnp
008600140212     c                   eval      ILV82nrs = t2nrs
008700140212     c                   eval      ILV82nsp = t2nsp
008800140212     c                   eval      ILV82tipa= tipa
008900140212     c                   call      'FNLV82R'
009000140212     c                   parm                    fnlv82ds
009100140212     c                   if        OLV82STS <>'9'
009200140206     c                   return
009300140206     c                   end
009400140206     c* scrivo la spia
009500140206     c                   clear                   rrnsia
009600140206     c     ksia          chain     fisia01l
009700140206     c* se il record è già presente da elaborare o elaborato esco dal pgm
009800140206     c                   if        %found(fisia01l) and
009900140206     c                             (siasts = '0' or
010000140206     c                             siasts = '1')
010100140206     c                   return
010200140206     c                   end
010300140206     c* se il record è già presente ma non ha avuto esito corretto tento
010400140206     c* di aggiornarlo
010500140206     c                   if        %found(fisia01l)
010600140206     c     rrnsia        chain(e)  FISIA00f
010700140206     c                   if        %error
010800140206     c                   return
010900140206     c                   end
011000140206     c                   end
011100140508      * salvo flag se esiste rekord
011200140508      *
011300140206     c                   clear                   FISIAds
011400140206     c                   time                    SIADAORI
011500140206     c                   eval      SIAAAS = t2aas
011600140206     c                   eval      SIALNP = t2lnp
011700140206     c                   eval      SIANRS = t2nrs
011800140206     c                   eval      SIANSP = t2nsp
011900140206     c                   eval      SIATIPA= tipa
012000140206     c                   eval      SIASTS ='0'
012100140212     c* Solo per Lasciato Avviso:
012200140212    1c                   if        t2chi<>'F'
012300140206     c* il pgm può essere richiamato sia in ricezione record CET se da PDA
012400140206     c* oppure da chiusura distinta
012500140207     c                   clear                   dsiaric
012600140207     c                   eval      §siachi = t2chi
012700140207     c                   eval      §siadev = t2dev
012800140207     c                   eval      §siahev = t2hev
012900140513     c                   eval      §siafd = §7rfd
013000140207     c                   eval      sianote= dsiaric
013100140212    1c                   endif
013200140206     c* se l'ho trovato ma è "anomalo" provo a rimandarlo
013300140206     c                   if        %found(fisia00f)
013400140206     c                   update    FISIA000                             01
013500140206     c                   else
013600140206     c                   write     FISIA000                             01
013700140206     c                   end
013800140206     c*
013900140206     C                   endsr
014000140206     c**********************************************************************
014100140206     c     cancello      begsr
014200140206     c**********************************************************************
014300140206     c* cancello la spia se non ancora elaborata
014400140206     c     ksia          chain     fisia01l
014500140206     c                   if        %found(fisia01l) and
014600140206     c                             siasts = '0'
014700140206     c     rrnsia        chain(e)  FISIA00f
014800140206     c                   if        %error
014900140206     c                   return
015000140206     c                   else
015100140206     c                   delete    FISIA000                             01
015200140206     c                   end
015300140206     c                   end
015400140206     c*
015500140206     C                   endsr
015600140213     c**********************************************************************
015700140213     c     routinz       begsr
015800140213     c**********************************************************************
015900140213     c* Imposto tipo alert a seconda della chiamata F=Fermo deposito  altrimenti RIC
016000140213     c                   if        t2chi<>'F'
016100140213     c                   eval      tipa='RIC'
016200140213     c                   else
016300140213     c                   eval      tipa='FD '
016400140213     c                   endif
016500140213     c                   endsr
016600140206     c**********************************************************************
016700140206     c     *inzsr        begsr
016800140206     c**********************************************************************
016900140206     c     *entry        plist
017000140206     c                   parm                    trtcp2ds
017100140212     c*
017200140206     c*
017300140206     c     kdst          klist
017400140206     c                   kfld                    cat4
017500140206     c                   kfld                    t2ndc
017600140206     c                   kfld                    t2fgs
017700140206     c*
017800140206     c     karb          klist
017900140206     c                   kfld                    t2aas
018000140206     c                   kfld                    t2lnp
018100140206     c                   kfld                    t2nrs
018200140206     c                   kfld                    t2nsp
018300140206     c*
018400140206     c     ksia          klist
018500140206     c                   kfld                    t2aas
018600140206     c                   kfld                    t2lnp
018700140206     c                   kfld                    t2nrs
018800140206     c                   kfld                    t2nsp
018900140206     c                   kfld                    TIPA
019000140508     c*
019100140508     c     ktab          klist
019200140508     c                   kfld                    tblkut
019300140508     c                   kfld                    tblcod
019400140508     c                   kfld                    tblkey
019500140508     c                   z-add     1             tblkut
019600140508     c                   movel     '7R'          tblcod
019700140206     c                   endsr
