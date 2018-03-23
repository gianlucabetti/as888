000100990610     H DECEDIT('0,') DATEDIT(*DMY.)
000200170420     H* FNLSA3R1*----------------------------------------------------*
000300970410     H*                                                              *
000400170420     H*       - Invio email di sospensione automatica oltre xx giorni*
000500170420     H*         (colli in partenza da + filiali)                     *
000600970410     H*--------------------------------------------------------------*
000601170420     Ffibsp03l  IF   e           k DISK
000800970410     F*--------
000900170509     FFNBLT01L  iF   E           K DISK
001000150204     FFNBrv07L  IF   E           K DISK
001300970410     F*--------
001400970410     FFNLBL01L  IF   E           K DISK
001500970410     F*--------
001600150219     FTABEL00F  IF   E           K DISK
001700970410     F*--------
001800150219     FAZORG01L  IF   e           k DISK
001900160125     F*--------
001901170503     FCNACO00F  IF   E           K DISK
001902170503
001903170427     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
002600970410     D*--------------------------------------------------------------*
002601170427      // - Comando di override al PrtF
002602170427     d C_CmdOvrPrtF    c                   const('OVRPRTF +
002603170427     d                                           file(PRTEMAIL) +
002604170427     d                                           pagesize(66 132) +
002605170427     d                                           lpi(6) cpi(10) +
002606170427     d                                           ovrscope(*actgrpdfn) +
002607170427     d                                           ')
002608170427     d C_CmdDltOvr     c                   const('DLTOVR +
002609170427     d                                            file(PRTEMAIL) +
002610170427     d                                            lvl(*actgrpdfn)')
002611170427      // - Tabella "MRA" = Bart-Mailing - Danni
002612170427     d dMRAdan       e ds                  inz
002613170427      // - Parametri x Ridefinizione dati utente estesi per mailing PDF
002614170427     d TRTCM1ds      e ds                  inz
002615170427      //   ·§CM1mitt = Indirizzo e-mail del mittente
002616170427     d   §CM1mitt    e                     inz(' ed@brt.it')
002617170427      //   ·§CM1dst  = Indirizzo e-mail del destinatario
002618170427     d   §CM1dst     e                     inz('rita.mazza@brt.it')
002619170427      //   ·§CM1tips = Tipo lettera via e-mail:
002620170427      //    "LET" = testo allegato in corpo con logo
002621170427      //            (richiede righe libere iniziali per il logo)
002622170427      //    "COI" = testo integrato senza logo senza BRT SPA iniziale
002623170427      //            (non consente né UNDERLINE né HIGHLIGHT)
002624170427     d   §CM1tips    e                     inz('COI')
002625170427      //   ·§CM1po   = Filiale
002626170427     d   §CM1po      e                     inz('046')
002627170427      //   ·§CM1var  = Oggetto e-mail
002628170427     d   §CM1var     e                     inz('*OBJM*+
002629170427     d                                     Manutenzione data spedizione+
002630170427     d                                      bolle ')
002631170427      //   ·§CM1sts  = Stato
002632170427     d   §CM1sts     e                     inz(*off)
002633170427      //   ·§CM1sts  = Id processo
002634170427     d   §CM1idp     e                     inz('2')
002700970410     D*  SCHIERE
002800970410     D*--------------------------------------------------------------*
004201170420
004202170420     d sbspl           s              3  0 dim(500)
004203170420     d sbspk           s              7  0 dim(500)
004204170427
004205170427     D s_BSPl          S              3  0 DIM(599)
004206170427     D s_BSPlBO        S              7  0 DIM(599)
004207170427     D s_BSPlD         S              8  0 DIM(599)
004208170427
004209170427     d ds_keyspel      ds                  occurs(599)
004210170515     d  s_kspe                       16    dim(10)
004211170427     d
004212170427     D s_BSPk          S              7  0 DIM(799)
004213170427     D s_BSPkBO        S              7  0 DIM(799)
004214170427     D s_BSPkD         S              8  0 DIM(799)
004215170427
004216170427     d ds_keyspek      ds                  occurs(799)
004217170515     d  s_kspek                      16    dim(10)
004218170503
004219170503     d ds_spe          ds
004220170503     d  ds_aas                        4
004221170503     d  ds_lnp                        3
004222170503     d  ds_nrs                        2
004223170503     d  ds_nsp                        7
004224170510     D PARAMa3         DS
004230170607     D  P_partfp               6      8
004231170607     D
004232170607     D
004300150204
004301170503     d O_testo         s            132    inz
004302170503     d wprimo          s              1
004700170427     D indy            S              3  0
004701170607     D partfp          S              3  0
004702170503     D Writerig        S              4  0 inz
004703170503     D x               S              3  0
004704170503     D y               S              3  0
004705170503     D i               S              3  0
005000150209     D wksc            S                   like(blpksc)
005500150217     d $Finerec        s               n   inz(*off)
005800150218     d wtfp            s                   LIKE(BLPTFP)
005801170510     d*partfp          s                   LIKE(BLPTFP)
005900150217
006000150217     D WrkStringaSql   S           4500
006100150217     D                                     VARYING
006101170503     D WrkStringaSql1  S           4500
006102170503     D                                     VARYING
006103170503     D Wrkfilin        S           4000
006104170503     D                                     VARYING
006105170503     D Wrkcliin        S           4000
006106170503     D                                     VARYING
006107170503     D Wrkcli          S           4000
006108170503     D                                     VARYING
006300150219     d data_eur        s               D   datfmt(*eur)
006301170503     d dataiso         s               D   datfmt(*iso)
006302170503     d datacur         s               D   datfmt(*iso)
006303170503     d Qcmd1           s            512    inz
006304170503     d Qlen            s             15  5 inz(150)
006400970410     D*--------------------------------------------------------------*
006500970410     D* DS
006600970410     D*--------------------------------------------------------------*
006601170427     d Tibs02Ds      e ds
006900150218     d fnlv55ds      e ds
007500150204     d WDAT8           DS
007600150204     d  datada                 1      8  0
007700150204     d  dataa                  9     16  0
007800150204     d  ggl                   17     21  0
007900080421      *
008000970410     D KPJBA         E DS
008100150217     d fnblpds       e ds                  extname(fnblp00f) inz
008200150204       // -?Parametri x Controllo profilo utenti?
008300150204     d TIBS34ds      e ds
008900150204       // -?Ds di riferimento al file esterno AZUTE00F?
009000150204     d AZUTEds       e ds                  extname(AZUTE00F)
009100150204       // -?Ds per dati organigramma?
009200150204     d dDATIUTE      e ds
012000150204     D ds3i          E DS
012001170427     D og140         E DS
012400040715
012500040715     d Fidn12ds      e ds
012600040715     d  skKey                 26    305    dim(20)
012700040715     d  skAnn                306    325    dim(20)
012800040715     d  skDca                326    485    dim(20)
012900040715     d  skFca                486    545  0 dim(20)
013000040715     d  skDch                546    705  0 dim(20)
013100040715     d  skCch                706    745    dim(20)
013200040715
013201170427      // - Esecuzione comando di sistema
013202170427     d qCmdExc         pr                  extpgm('QCMDEXC')
013203170427     d  Qcmd1                       512    const  options(*varsize)
013204170427     d  Qlen                         15  5 const
013300970410     D                SDS
013400970410     D  NOMPGM                 1     10
013401170503     d   JobUser             254    263
013402170503      /copy gaitrasrc/srcprotopr,fnlv55r
013600150211
013700970410     C*--------------------------------------------------------------*
013800970410     C*  C I C L O     P R I N C I P A L E
013900970410     C*--------------------------------------------------------------*
014000150217      /free
014100150217         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014101170420
014300170420      * caricamento schiere lnp e clienti con sospensione bolle e colli in
014301170420      * partenza da più filiali
014302170420         exsr sr_fibsp;
014303170420
014304170427        if sbspl(1)>0 ;
014305170427         exsr sr_elaboraL ;
014306170427         exec sql close BLPcsr;
014307170420        endif;
014308170427        if sbspk(1)>0 ;
014309170503         exsr sr_elaboraK ;
014310170427         exec sql close BLPcsr1;
014311170420        endif;
014312170427
014313170427     c*  Se ho memorizzato qualcosa invio email
014314170427        if s_bspl(1)>0 or s_bspk(1)>0      ;
014315170427           exsr INV_email                  ;
014316170427        endif                              ;
014600080421      *
015405161114
015500970410     C                   SETON                                        LR
015501170420       //-------------------------------------------------------------*
015502170420       //Lettura FIBSP record da elaborare  X MEMORIZZAZIONE schiere  *
015503170420       //-------------------------------------------------------------*
015504170420       Begsr SR_fibsp  ;
015505170420
015507170420        y=0 ;
015508170503        x=0 ;
015509170420        clear  sbspl;
015510170420        clear  sbspK;
015511170420
015512170420        setll 'S' fibsp03l;
015513170420        reade 'S' fibsp03l;
015514170420
015515170420  1     dow not %eof(fibsp03l) ;
015516170420
015517170420          if bspfil>0;
015518170420        // Determino il terminal di partenza della filiale
015519170420             exsr sr_terfil;
015520170420             if wtfp = partfp;
015521170420                x+=1;
015522170503                sbspl(x)=bspfil ;
015523170420             endif;
015524170420          else;
015525170420                y+=1;
015526170420                sbspk(y)=bspksc ;
015527170420          endif;
015557170420           reade 'S'    fibsp03l ;
015558170420        enddo;
015559170420
015560170420        endsr;
015561170420       //-----------------------------------------------------------------
015562170503       // Terminal di partenza della linea di partenza
015563170420       //-----------------------------------------------------------------
015564170420       begsr sr_terfil;
015565170420             clear wtfp;
015566170420             reset fnlv55ds;
015567170420             D55lin = bspfil;
015568170420             d55drf = %dec(datacur:*iso);
015569170420             d55tpt = 'P';
015570170420             callp fnlv55r(fnlv55ds);
015571170420             if d55err = ' ';
015572170420                wtfp=d55tfp;
015573170420             endif;
015574170420       endsr;
015575170427       //--------------------------------------------------------------
015576170427       //?elaborazione per linea
015577170427       //--------------------------------------------------------------
015578170427       BEGSR sr_elaboraL;
015579170427     c                   clear                   s_bspl
015580170427     c                   clear                   s_bsplBO
015581170427     c                   clear                   s_bsplD
015582170427     c                   do        599           indy
015583170427     c     indy          occur     ds_keyspel
015584170427     c                   clear                   ds_keyspel
015585170427     c                   enddo
015586170427     c                   clear                   indy
015587170427          exsr sr_prepsql;
015588170427           $finerec=*off;
015589170427           exec sql prepare STRINGASQL from :WrkStringaSql;
015590170427           exec sql declare BLPCSR cursor for StringaSql;
015591170427           exec sql open BLPcsr;
015592170427           dow $finerec=*off;
015593170427              exec sql Fetch BLPcsr into :fnblpds;
015594170427              if sqlcod=100 or sqlcod<0;
015595170427                 $finerec = *on;
015596170427                 leave;
015597170427              endif;
015598170427                 *in28=*off ;
015599170427                 exsr ctr01_uni;
015600170427                 if not *in28;
015601170427       // record da segnalare:
015602170427     c                   eval      indy=%lookup(blplnp:s_bspL)
015603170427    2c                   if        indy=0
015604170427     c                   eval      indy=%lookup(0:s_bspL)
015605170427     c* memorizzo cod cliente
015606170427     c                   eval      s_bspL(indy)=blplnp
015607170427    2c                   endif
015608170427     c* memorizzo numero di spedizioni in sosp oltre xx giorni
015609170427     c                   add       1             s_bspLbo(indy)
015610170427     c* memorizzo la data più bassa di immissione
015611170427    2c                   if        s_BSPlD(indy)=0 or
015612170427     c                             blpdim<s_BSPlD(indy)
015613170427     c                   eval      s_BSPlD(indy)=blpdim
015614170427    2c                   endif
015615170515     c* memorizzo fino a 10 key spedizioni
015616170515     c                   if        s_bspLbo(indy)<=%elem(s_kspe)
015617170427     c     indy          occur     ds_keyspel
015618170427     c                   eval      s_kspe(s_bspLbo(indy))=%editc(blpaas:'X') +
015619170427     c                              %editc(blplnp:'X') + %editc(blpnrs:'X') +
015620170504     c                              %editc(blpnsp:'Z')
015621170427    2c                   endif
015623170503                 endif;
015625170427           enddo;
015626170427       ENDSR  ;
015627170427       //--------------------------------------------------------------
015628170427       //?Preparazione stringa SQL
015629170427       //--------------------------------------------------------------
015630170427       BEGSR sr_prepsql;
015631170427       WrkStringaSql='select * from fnblp00f' ;
015632170427          exsr sr_whrfil;
015633170427          WrkStringaSql=WrkStringaSql+WrkFilIn +
015635170427       ' and blptfp=' + %editc(partfp:'X') +
015636170427       ' and blpnfv=0' +
015637170427       ' order by  blptfp, blpnfv';
015638170427       endsr;
015639170427       //-------------------------------------------------------------*
015640170427       //Schiera filiali con sospensione automatica                 . *
015641170427       //-------------------------------------------------------------*
015642170427       Begsr SR_whrfil;
015643170503          wPrimo= 'S' ;
015644170427          reset WrkFilIn;
015645170427          WrkFilIn= ' where blplnp in (';
015646170427          for I=1 to %elem(sbspl);
015647170427             if sbspl(i)=0;
015648170427                leave;
015649170427             endif;
015650170427
015651170503               if wprimo<>'S' ;
015652170427                 WrkFilIn=WrkFilIn + ' , ';
015653170427               endif;
015654170427             WrkFilIn=WrkFilIn + %char(sbspl(i));
015655170503             clear wPrimo   ;
015657170427          endfor;
015658170427          WrkFilIn=WrkFilIn + ')';
015659170427          endsr;
015660170427       //--------------------------------------------------------------
015661170427       //?elaborazione per cliente
015662170427       //--------------------------------------------------------------
015663170427       BEGSR sr_elaborak;
015664170427     c                   clear                   s_bspk
015665170427     c                   clear                   s_bspkBO
015666170427     c                   clear                   s_bspkD
015667170427     c                   do        799           indy
015668170427     c     indy          occur     ds_keyspek
015669170427     c                   clear                   ds_keyspek
015670170427     c                   enddo
015671170427     c                   clear                   indy
015672170427          exsr sr_prepsqlk;
015673170427           $finerec=*off;
015674170503           exec sql prepare STRINGASQL1 from :WrkStringaSql1;
015675170503           exec sql declare BLPCSR1 cursor for StringaSql1;
015676170427           exec sql open BLPcsr1;
015677170427           dow $finerec=*off;
015678170427              exec sql Fetch BLPcsr1 into :fnblpds;
015679170427              if sqlcod=100 or sqlcod<0;
015680170427                 $finerec = *on;
015681170427                 leave;
015682170427              endif;
015683170427                 *in28=*off ;
015684170427                 exsr ctr01_uni;
015685170427                 if not *in28;
015686170427       // record da segnalare:
015687170427     c                   eval      wksc=blpksc
015688170427     c                   if        blpccm>0
015689170427     c                   eval      wksc=blpccm
015690170427     c                   endif
015691170427     c                   eval      indy=%lookup(wksc:s_bspK)
015692170427    2c                   if        indy=0
015693170427     c                   eval      indy=%lookup(0:s_bspK)
015694170427     c* memorizzo cod cliente
015695170427     c                   eval      s_bspK(indy)=wksc
015696170427    2c                   endif
015697170427     c* memorizzo numero di spedizioni in sosp oltre xx giorni
015698170427     c                   add       1             s_bspKbo(indy)
015699170427     c* memorizzo la data più bassa di immissione
015700170427    2c                   if        s_BSPkD(indy)=0 or
015701170427     c                             blpdim<s_BSPkD(indy)
015702170427     c                   eval      s_BSPkD(indy)=blpdim
015703170427    2c                   endif
015704170427     c* memorizzo fino a 5 key spedizioni
015705170515     c                   if        s_bspkbo(indy)<=%elem(s_kspek)
015706170427     c     indy          occur     ds_keyspek
015707170427     c                   eval      s_kspek(s_bspkbo(indy))=%editc(blpaas:'X') +
015708170427     c                              %editc(blplnp:'X') + %editc(blpnrs:'X') +
015709170504     c                              %editc(blpnsp:'Z')
015710170427    2c                   endif
015712170427                 endif;
015713170427           enddo;
015714170427       ENDSR  ;
015715170427       //--------------------------------------------------------------
015716170427       //?Preparazione stringa SQL
015717170427       //--------------------------------------------------------------
015718170427       BEGSR sr_prepsqlk;
015719170503       WrkStringaSql1='select * from fnblp00f' ;
015720170427          exsr sr_whrcli;
015721170503          WrkStringaSql1=WrkStringaSql1+WrkCliIn +
015722170427       ' and blptfp=' + %editc(partfp:'X') +
015723170427       ' and blpnfv=0' +
015724170427       ' order by  blptfp, blpnfv ';
015725170427       endsr;
015745170427       //-------------------------------------------------------------*
015746170427       //Schiera codici clienti da elaborare               *
015747170427       //-------------------------------------------------------------*
015748170427       Begsr SR_whrcli;
015749170427       wprimo='S';
015750170427       reset WrkCliIn;
015751170427       reset WrkCli  ;
015755170427        WrkcliIn= ' where blpccm in (';
015757170427          for I=1 to %elem(sbspk);
015758170427             if sbspk(i)>0;
015759170427                if wprimo<>'S';
015760170427                   Wrkcli=Wrkcli + ', ';
015761170427                endif;
015762170503                WrkCLI=WrkClI + %editc(sbspk(i):'X')  ;
015763170427                wprimo=' ';
015764170427             endif;
015765170427          endfor;
015766170427       wrkcli=wrkcli + ')';
015767170427       WrkCliIn=WrkCliIn + WrkCli + 'or blpksc in (';
015768170427       WrkCliIn=WrkCliIn + WrkCli ;
015772170427       endsr;
015773170427     C*--------------------------------------------------------------*
015774170427     C*    Controlli unici per tutti i tipi di entrata
015775170427     C*--------------------------------------------------------------*
015776170427     C     ctr01_uni     BEGSR
015777170427     C* Controllo se bolla figlia
015778170427     C                   Z-ADD     blpAAS        KAAS
015779170427     C                   Z-ADD     blpLNP        KLNP
015780170427     C                   Z-ADD     blpNRS        KNRS
015781170427     C                   Z-ADD     blpnsp        KNSP
015782170427     C     KBLP          CHAIN     FNLBL01L
015783170427     C                   if        %found(fnlbl01l)
015784170427     C                   SETON                                        28
015785170427     c                   leavesr
015786170427     C                   END
015787170427     C* Controllo che non sia stata data per consegnata
015788170427     C     BLPDCM        IFNE      0
015789170427     C     BLPCCA        ANDNE     '6'
015790170427     C     BLPDCM        ORNE      0
015791170427     C     BLPCCA        ANDNE     '2'
015792170427     C                   SETON                                        28
015793170427     c                   leavesr
015794170427     C                   END
015795170427     C* Controllo che abbia il dettaglio segnacolli
015796170504     c* in questo pgm questo controllo non va bene perchè altrimenti
015797170504     c* non vengono segnalate le bolle "zoppe" quelle per cui la conferma
015798170504     c* si è interrotta senza creare il dettaglio colli. Per queste bisogna
015799170504     c* verificare che esista un'altra bolla uguale e poi annullare la zoppa
015800170504     C**   KBLT          CHAIN(n)  FNBLT01L
015801170504     C**                 IF        not %found(fnblt01l)
015802170504     C**                 SETON                                        28
015803170504     c**                 leavesr
015804170504     C**                 END
015805170427     C* Controllo che non sia stata data per partita
015806170427     C     BLPDT1        IFNE      0
015807170427     C                   SETON                                        28
015808170427     c                   leavesr
015809170427     C                   END
015810170427     C* Controllo che non sia stata trasmessa in sede
015811170427     C     BLPDT2        IFNE      0
015812170427     C                   SETON                                        28
015813170427     c                   leavesr
015814170427     C                   END
015815170427     C* Controllo che non sia stata trasmessa al cliente
015816170427     C     BLPDT3        IFNE      0
015817170427     C                   SETON                                        28
015818170427     c                   leavesr
015819170427     C                   END
015820170427      * Controllo C.A.
015821170427      *
015822170427     c                   clear                   fidn12ds
015823170427     c                   eval      i12aas = blpaAS
015824170427     c                   eval      i12lnp = blpLNP
015825170427     c                   eval      i12nrs = blpNRS
015826170427     c                   eval      i12nsp = blpNSP
015827170427     c                   eval      i12fel = 'E'
015828170427     c                   eval      i12fan = 'E'
015829170427     c                   eval      i12fch = 'E'
015830170427     c                   call      'FIDN12R'
015831170427     c                   parm                    fidn12ds
015832170427      **
015833170427      * se c'è almeno una C.A. aperta
015834170427     c     o12nca        ifgt      0
015835170427     C                   SETON                                        28
015836170427     c                   leavesr
015837170427     c                   end
015838170427     c* controllo che la spedizione non abbia spunte
015839170427     c                   clear                   wsispu            1
015840170427     c     kblt          setll     fnblt01l
015841170509     c     kblt          reade     fnblt01l                               99
015842170427    2c                   dow       not *in99
015843170427     c
015844170427     c* Cerco una qualsiasi spunta
015845170427     c     kbrv          chain     fnbrv07l
015846170427    3c                   if        %found(fnbrv07l)
015847170427     c                   eval      wsispu='S'
015848170427     c                   seton                                        99
015849170427   x3c                   else
015850170509     c     kblt          reade     fnblt01l                               99
015851170427    3c                   endif
015852170427    2c                   enddo
015853170427     c*
015854170427    2c                   if        wsispu='S'
015855170427     c                   seton                                        28
015856170427     c                   leavesr
015857170427     c                   endif
015858170427     c
015859170427     c* la segnalazione avviene solo se
015860170427     c* trascorsi i giorni previsti in tabella
015861170427     c                   clear                   wdat8
015862170427     c                   z-add     blpdim        datada
015863170427     c                   eval      dataa=(blpaas*10000)+blpmgs
015864170427     c                   call      'XSRLAV8'
015865170427     c                   parm                    wdat8
015866170427     c                   if        (ggl-1) <= §3iggm
015867170427     c                   seton                                        28
015868170427     c                   leavesr
015869170427     c                   endif
015870170427     c
015871170427     c                   endsr
015872170427     c*-------------------------------------------------------------------
015873170427     c     INV_email     BEGSR
015874170427     c
015875170427     c                   clear                   writerig
015876170427     c                   if        not %open(prtemail)
015877170427     C                   EXSR      SR_OPENPRTF
015878170427     C                   endif
015879170427     c
015880170427     c                   exsr      Routend
015881170427     c
015882170427     c                   ENDSR
015883170427       //--------------------------------------------------------------
015884170427       //?Override al file di stampa per impostarvi i dati per
015885170427       //?  l'invio via e-mail   +   stampa inizio e-mail
015886170427       //--------------------------------------------------------------
015887170427       BEGSR sr_OpenPrtF;
015888170427
015889170427         // Override al file di stampa per impostarvi i dati per
015890170427         // l'invio via e-mail
015891170427           exsr sr_Override;
015892170427
015893170427
015894170427           indy = 1 ;
015895170427
015896170427          dow     s_BSPl(indy)>0  ;
015897170427
015898170427          if      s_BSPlBO(indy)>0 ;
015899170427          dataiso=%date(s_BSPlD(indy)) ;
015900170427          data_eur=dataiso  ;
015901170427
015902170427           clear O_testo;
015903170427           chain  s_BSPl(indy)   azorg01l  ;
015904170427           if not %found(azorg01l)  ;
015905170427           eval orgdes=*all'?'  ;
015906170427           endif  ;
015907170427           %subst(O_testo:6:20) = %editc(s_BSPl(indy):'X')+'-'+orgdes  ;
015908170427           %subst(O_testo:46:10) =%char(data_eur)  ;
015909170504          if s_bsplBo(indy)<=%elem(s_kspe);
015910170503
015911170503     c     indy          occur     ds_keyspel
015912170503
015913170504           for y=1 to s_bsplBo(indy);
015914170503             if y>1;
015915170503                clear O_testo;
015916170503             endif;
015917170503             eval ds_spe=s_kspe(y);
015918170503             %subst(O_testo:64:3 ) =ds_lnp  ;
015919170503             if ds_nrs>'00';
015920170503               %subst(O_testo:68:2 ) =ds_nrs  ;
015921170503               %subst(O_testo:70:1 ) ='-'                    ;
015922170503             endif;
015923170503             %subst(O_testo:71:7 ) =ds_nsp  ;
015924170503             %subst(O_testo:79:4 ) =ds_aas  ;
015925170503             except PrtDet;
015926170503             writerig=writerig+1  ;
015927170503             if writerig>=50  ;
015928170503               exsr  sr_50righe;
015929170503             endif  ;
015930170503           endfor;
015931170503          else;
015932170427           %subst(O_testo:64:9 ) =%editc(s_BSPlBO(indy):'2')  ;
015933170427           except PrtDet;
015934170427           writerig=writerig+1  ;
015935170503          endif;
015936170427
015937170427           // se ne ho scritte 50 di righe devo inviare
015938170427           if writerig>=50  ;
015939170503             exsr  sr_50righe;
015940170427           endif  ;
015941170503           endif  ;
015942170427
015943170427           indy=indy+1  ;
015944170427           enddo    ;
015945170427
015946170427          // Stesso ciclo sui cod clienti
015947170427          indy = 1      ;
015948170427          dow     s_BSPk(indy)>0  ;
015949170427
015950170427          if      s_BSPkBO(indy)>0 ;
015951170427          dataiso=%date(s_BSPkD(indy)) ;
015952170427          data_eur=dataiso  ;
015953170427
015954170427           clear O_testo;
015955170503           chain  (1:dutkci:s_BSPk(indy)) cnaco00f  ;
015956170427           if not %found(cnaco00f)  ;
015957170427           eval acorag=*all'?'  ;
015958170427           endif  ;
015959170427           %subst(O_testo:6:20) = %editc(s_BSPk(indy):'X')+'-'+acorag  ;
015960170427           %subst(O_testo:46:10) =%char(data_eur)  ;
015961170504          if s_bspkBo(indy)<=%elem(s_kspek);
015962170503     c     indy          occur     ds_keyspek
015963170504           for y=1 to s_bspkBo(indy);
015964170503             if y>1;
015965170503                clear O_testo;
015966170503             endif;
015967170503             eval ds_spe=s_kspek(y);
015968170503             %subst(O_testo:64:3 ) =ds_lnp  ;
015969170503             if ds_nrs>'00';
015970170503               %subst(O_testo:68:2 ) =ds_nrs  ;
015971170503               %subst(O_testo:70:1 ) ='-'                    ;
015972170503             endif;
015973170503             %subst(O_testo:71:7 ) =ds_nsp  ;
015974170503             %subst(O_testo:79:4 ) =ds_aas  ;
015975170503             except PrtDet;
015976170503             writerig=writerig+1  ;
015977170503             if writerig>=50  ;
015978170503               exsr  sr_50righe;
015979170503             endif  ;
015980170503           endfor;
015981170503          else;
015982170427           %subst(O_testo:64:9 ) =%editc(s_BSPkBO(indy):'2')  ;
015983170427           except PrtDet;
015984170427           writerig=writerig+1  ;
015985170427           endif  ;
015986170427
015987170427           // se ne ho scritte 50 di righe devo inviare
015988170427           if writerig>=50  ;
015989170503             exsr  sr_50righe   ;
015990170427           endif  ;
015991170503           endif  ;
015992170427
015993170427           indy= indy+1  ;
015994170427           enddo    ;
015995170427
015996170427             exsr      Routend  ;
015997170427       ENDSR;
015998170427
015999170427       //--------------------------------------------------------------
016000170427       //?Override al file di stampa per impostarvi i dati per
016001170427       //?  l'invio via e-mail   +   stampa inizio e-mail
016002170427       //--------------------------------------------------------------
016003170427       BEGSR sr_Override;
016004170427
016005170427         reset TRTCM1ds;
016006170427
016007170427         IF  §MRAdreg <> *blank;
016008170427           §CM1mitt = %trim(§MRAdmitt);
016009170509         // La seguente specifica sarà da ripristinare al termine dei test
016010170509         //  §CM1dst  = %trim(§MRAddest);
016011170427
016012170427         // Valorizzo i campi della e-m@ail
016013170503          chain  partfp   azorg01l ;
016014170427
016015170427          if   %found(azorg01l) ;
016016170427           og140=orgde0  ;
016017170427           if §ogapo>*zeros  ;
016018170509         // La seguente specifica sarà da ripristinare al termine dei test
016019170509         //§cm1dst=%trim(§cm1dst) +'; CED'+ §ogapo +'@brt.it;';
016020170427           else  ;
016021170509         // La seguente specifica sarà da ripristinare al termine dei test
016022170509         //§cm1dst=%trim(§cm1dst) +'; CED'+%editc(orgf70:'X')+'@brt.it;';
016023170427          endif  ;
016024170427          endif  ;
016025170427
016026170427           §CM1tips = §MRAdreg;
016027170503           §CM1po   = %editc(partfp:'X');
016028170427           §CM1var  = '*OBJM*' + 'SOSPENSIONE AUTOMATICA bolle      '   ;
016029170427           §CM1idp  = §MRAdidpro;
016030170427           Qcmd1 = C_CmdOvrPrtF
016031170427                + ' outq(' + %trim(§MRAdoutqi) + ')'
016032170427                + ' usrdfndta(''' + TRTCM1ds + ''')';
016033170427         ELSE;
016034170427           Qcmd1 = C_CmdOvrPrtF;
016035170427         ENDIF;
016036170427
016037170427         callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
016038170427
016039170427           open PrtEMAIL;
016040170427
016041170427         // Stampa una testata se NON è richiesta la e-mail
016042170427           IF  §MRAdreg =  *blank;
016043170427             O_testo = JobUser + ' - ' + nompgm
016044170427                     + ' - ' + %editc( *date : 'Y' )
016045170427                     + ' - *REM* ' + %subst(§CM1var : 7 : 70);
016046170427             except PrtDet;
016047170427             clear O_testo;
016048170427             except PrtDet;
016049170427             except PrtDet;
016050170427           ENDIF;
016051170427
016052170427         // Stampa testo iniziale
016053170427           O_testo = 'E'' stata eseguita la Sospensione Automatica +
016054170427                      di bolle immesse da almeno xx giorni.' ;
016055170427           %subst(O_testo:72:2) = (%editc(§3iggm: '4'))     ;
016056170427           except PrtDet;
016057170427           O_testo = 'Verificare col mittente o procedere alla lor+
016058170427                      o chiusura come "Merce MAI Affidata".' ;
016059170427           except PrtDet;
016060170427
016061170427         // Stampa una riga vuota
016062170427           clear O_testo;
016063170427           except PrtDet;
016064170427
016065170427         // Stampa intestazione
016066170427           O_testo = ' Bolle spostate in sospensione di          Data imm+
016067170427                     issione    Bolle da verificare       ';
016068170427           except PrtDet;
016069170427           O_testo = 'Codice Cliente o Linea di Partenza          più vec+
016070170427                     chia                                 ';
016071170427         //%subst(o_testo:90:10)= knmus     ;
016072170427           except PrtDet;
016073170427
016074170427         // Stampa una riga vuota
016075170427           clear O_testo;
016076170427           except PrtDet;
016077170427       ENDSR;
016078170503       //--------------------------------------------------------------
016079170503       //?Operazioni finali.
016080170503       //--------------------------------------------------------------
016081170503       BEGSR sr_50righe;
016082170503             exsr      Routend  ;
016083170503             clear writerig                  ;
016084170503
016085170503             exsr sr_Override;
016086170503       ENDSR;
016087170427
016088170427       //--------------------------------------------------------------
016089170427       //?Operazioni finali.
016090170427       //--------------------------------------------------------------
016091170427       BEGSR RoutEnd;
016092170427
016093170427         if %open(PrtEMAIL);
016094170427
016095170427         //?Chiusura dello spool?
016096170427           clear O_testo;
016097170427           except PrtDet;
016098170427           O_testo = '***   Fine Lista   ***';
016099170427           except PrtDet;
016100170427
016101170427           close PrtEMAIL;
016102170427
016103170427         //?Eliminazione overflow?
016104170427           Qcmd1 = C_CmdDltOvr;
016105170427           callp(e) qCmdExc(Qcmd1 : %size(Qcmd1));
016106170427
016107170427         endif;
016108170427
016109170427       ENDSR;
099600970410     C*--------------------------------------------------------------*
099700970410     C* *INZSR: Operazioni iniziali
099800970410     C*--------------------------------------------------------------*
099900970410     C     *INZSR        BEGSR
100000970410     C*
100100970410     C     *ENTRY        PLIST
100200970410     C                   PARM                    KPJBA
100201170510     c                   movel     kpjbu         parama3
100300150204
100400150204     c     *dtaara       define    §azute        AZUTEds
100500150204     c     *dtaara       define    §datiute      dDATIUTE
100600150204     c                   in(E)     *dtaara
100700150204     c                   if        %ERROR or RSUT = *blanks
100800150204     c                   clear                   Tibs34Ds
100900150204     c                   call      'TIBS34R'
101000150204     c                   parm                    Tibs34Ds
101100150204     c                   in        *dtaara
101200150204     c                   endif
101300150204      *
101400150204     c                   z-add     1             CodUt             1 0
101401170503             datacur=%date();
101402170607      * Terminal di partenza: se non passato in kpjbu lo determino da knmus
101403170607         if p_partfp>*zeros;
101406170607           partfp=%int(p_partfp);
101407170607         else;
101408170607           partfp=simfel;
101409170607         endif;
101410170607
101500030924      *
101600030924      *
101700970410      *  Definizione chiavi
101800970410     C     KBLP          KLIST
101900970410     C                   KFLD                    KAAS
102000970410     C                   KFLD                    KLNP
102100970410     C                   KFLD                    KNRS
102200970410     C                   KFLD                    KNSP
102300970410     C     KBLT          KLIST
102400150204     C                   KFLD                    blpaas
102500150204     C                   KFLD                    blplnp
102600150204     C                   KFLD                    blpnrs
102700150204     C                   KFLD                    blpnsp
103700150204     C     KBrv          KLIST
103800150204     C                   KFLD                    bltfls
103900150204     C                   KFLD                    bltlna
104000150204     C                   KFLD                    bltNRS
104100150204     C                   KFLD                    bltnsc
104500970410     C     KTAB2         KLIST
104600970410     C                   KFLD                    KKUT
104700970410     C                   KFLD                    KCOD
104800970410     C                   KFLD                    KKEY
104801170504     c                   z-add     1             kkut
104900970410      *  Definizione variabili
105000970410     C     *LIKE         DEFINE    BLPAAS        KAAS
105100970410     C     *LIKE         DEFINE    BLPLNP        KLNP
105200970410     C     *LIKE         DEFINE    BLPNRS        KNRS
105300970410     C     *LIKE         DEFINE    BLPNSP        KNSP
105400970410     C     *LIKE         DEFINE    TBLKUT        KKUT
105500970410     C     *LIKE         DEFINE    TBLCOD        KCOD
105600970410     C     *LIKE         DEFINE    TBLKEY        KKEY
105700970410      *
109600150204     C***
109700150204     C                   MOVEL     '3I'          Kcod
109800150204     C                   MOVEL     '1       '    kKEY
109900150204     c                   clear                   ds3i
110000150204     C     KTAB2         CHAIN     TABEL00F
110100150205     c                   if        %found(tabel00f)
110200150204     C                   MOVEL     TBLUNI        DS3I
110300150204     c                   endif
110301170427
110302170427     c                   Clear                   tibs02ds
110303170427     c                   Clear                   dmradan
110304170427     c                   Movel     'C'           T02Mod
110305170427     c                   Movel     KNSIF         T02Sif
110306170427     c                   Movel     'MRA'         T02Cod
110307170427     c                   Movel     'FNLSU3R'     T02Ke1
110308170427     c                   Call      'TIBS02R'
110309170427     c                   Parm                    Kpjba
110310170427     c                   Parm                    tibs02ds
110311170427    2c                   If        T02Err = *Blanks
110312170427     c                   Movel     T02Uni        Dmradan
110313170427    2c                   endif
112300970410     C*
112400970410     C                   ENDSR
112500080930      *------------------------------------------------------------------------*
112600080930      * Routine di controllo x eventuali Errori
112700080930      *------------------------------------------------------------------------*
112800080930     C     *PSSR         BEGSR
112900080930      *
113000080930     C                   ENDSR     '*CANCL'
113100080930     C*------------------------------------------------------------------------*
113200170427     o* ---------------------------------------------------------------
113300170427     o* ?Spool di stampa (per e-mail).
113400170427     o* ---------------------------------------------------------------
113500170427     oPrtEMAIL  e            PRTdet      1
113600170427     o                       O_testo
