000100970910     H*------------------------------------------------------------------------*
000200130905     H* CAPPARIO - STAMPA CAPPARIO: ORARI SERVIZI
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000701130905     fAZORG01L  if   e           k disk
000702130909     ftabel00f  if   e           k disk
000800130905     FTISIo5D   CF   E             WORKSTN
000801130906
001900130906     D MSG             S             78    DIM(7) CTDATA PERRCD(1)
001901130906
001902130906     d*
001903130905     d v1cdriinv       s                   like(v1cdri)
001904130909     d wkey            s                   like(tblkey)
001905130905     d datacur         s               d   datfmt(*iso)
001906130905     d dataiso         s               d   datfmt(*iso)
001907130905     d dataeur         s               d   datfmt(*eur)
001908131018     d w0080           s              8  0
001909130905      // ricerca filiale su organigramma
001910130905     d   pInFIL        S              3
001911130905     d   pInFAG        S              1
001912130905     d   pInDES        S             25
001913130905     d   pInDIT        S              3
001914130906      // ricerca versione cappario
001915130906     d p96dri          s              8  0
001916130906     d p96ver          s              5  0
001917130905
001918130905     d $Fine           s               n   inz(*off)
001919130905     d $InzD01         s               n   inz(*on)
010500130906     D
010600970910     D KPJBA         E DS
010700061017      * - Parametri x Controllo profilo utenti
010800061017     d TIBS34ds      e ds
010900061017      * - Ds di riferimento al file esterno AZUTE00F
011000130905     d §azute        e ds                  extname(AZUTE00F)
011001130905     d                                     dtaara
011200130905     d §datiute      e ds                  extname(ddatiute)
011201130905     d                                     dtaara
011202130905     d dssi00        E DS                  EXTNAME(tisi00ds)                    *ds di procedura
011203130906     d tisio6ds      E DS
011204130909     d ds15          E DS
011205130905     d wlbda8          DS                  INZ                                  *controlla data
011206130905     d  g08dat                        8  0
011207130905     d  g08inv                        8  0
011208130905     d  g08err                        1
011209130905     d  g08tgi                        5  0
011210130905       // -?File AZORG01L?
011211130905     d k01azorg01    e ds                  extname(AZORG01L : *key)
011212130905     d                                     prefix(k_)   inz
011213130905       // -?Costante per controllo "caratteri solo numerici"?
011214130905     d c_Digits        c                   const('0123456789')
011215130906
011216130906       // - Lancio batch
011217130906     d bch10           pr                  extpgm('BCH10')
011218130906     d   kpjba                             likeds(KPJBA)
011219130905      //---------------------------------------------------------------
011220130905      //?Definizione procedure usate.
011221130905      //---------------------------------------------------------------
011222130905      /copy gaitrasrc/srcprotopr,tibs34r
011223130905      /copy gaitrasrc/srcprotopr,tnsd24r
011224130906      /copy gaitrasrc/srcprotopr,tisi96r
011300970910     C*------------------------------------------------------------------------*
011400970910     C* MAIN LINES
011500970910     C*------------------------------------------------------------------------*
011501130905     c     *Entry        plist
011502130905     c                   parm                    KPJBA
011503131018     c                   parm                    datarif           8
011504130905      /free
011505130905
011506130905       // Operazioni iniziali
011507130905       exsr RoutInz;
011508130905
011510130905       // Gestione video
011511130905           DOW $Fine = *off;
011516130905             exsr GesD01;
011521130905           ENDDO;
011522130905
011525130905       // Operazioni finali
011526130905       exsr RoutEnd;
011527130905       //--------------------------------------------------------------
011528130905       //?Gestione Videata
011529130905       //--------------------------------------------------------------
011530130905       BEGSR GesD01;
011531130905
011532130905         // Inizializzazione videata
011533130905         if  $InzD01 = *on;
011534130905            exsr Inzd01;
011539130905            $Inzd01  = *off;
011540130905         endif;
011541130905
011542130905
011558130905         // Emissione videata
011559130905         exfmt  sio5d01;
011560130905
011561130905         *in28=*off;
011562130905         *in90=*off;
011563130905         clear V1dmsg;
011575130905
011576130905 1       SELECT;
011577130905
011578130905         // - F3=Fine
011579130905 1         WHEN  *inkc      ;
011580130905            $Fine = *on;
011617130905         // - F10=Ricerca versioni cappario
011618130905 1         WHEN  *inkj;
011619130905              exsr f10ges;
011638130905         // - F6=Conferma
011639130905 1         WHEN  *inkf;
011640130905            exsr  ctrd01;
011641130905            if not *in90;
011642130905               exsr  confe;
011643130905               $Fine =*on;
011644130905            endif;
011645130905           OTHER;
011646130905         // - ENTER - Controlli videata
011647130905            exsr  ctrd01;
011666130905 1       ENDSL;
011667130905
011668130905       ENDSR;
011670130905       //--------------------------------------------------------------
011671130905       //?Inizializzazione videata
011672130905       //--------------------------------------------------------------
011673130905       BEGSR inzd01;
011674130905         *in70=*off;
011675130905         *in71=*off;
011676130905         *in72=*off;
011677130905         *in74=*off;
011678130905         *in28=*off;
011685131018            // data riferimento - Propongo data del giorno
011686131018            v1cdri=%dec(datacur:*eur);
011688131018         if %parms = 2;
011689131018      /end-free
011690131018     c                   testn                   datarif              30
011691131018      /free
011692131018            if        *in30 and %subst(datarif:8:1)>='0' ;
011693131018               w0080=%int(datarif);
011695131018            // controllo la data ricevuta e se ok la metto nella data riferimento del video
011696131018               test(DE) *iso w0080  ;
011697131018               if not %error;
011698131018                  dataeur=%date(w0080  :*iso);
011699131018                  v1cdri=%dec(dataeur);
011700131018               endif;
011701131018            endif;
011702131018         endif;
011703130909         // Visualizzo la nazione che per il momento è fissa Italia
011704130909         clear v1cnar;
011705130909         clear v1dnar;
011706130909         clear wkey;
011707130909         chain (1:'15':wkey) tabel00f;
011708130909         if %found(tabel00f) and tblflg=*blanks;
011709130909            ds15=tbluni;
011710130909            v1dnar=§15des;
011711130909         endif;
011712130905         clear v1cca1;
011713130905         clear v1cca2;
011714130905         clear v1clna;
011715130905         clear v1dlna;
011716130905       endsr;
011717130905       //--------------------------------------------------------------
011718130905       //?F10=Ricerca Versioni
011719130905       //--------------------------------------------------------------
011720130905       BEGSR f10ges;
011721130905      /end-free
011722130905     c                   setoff                                       72
011723130905     C* lancia il programma di gestione versioni in ricerca
011724130905     c                   CLEAR                   dssi00                         *pulisce ds
011725130905     c                   MOVEL     'R01'         d00op0                         *livello procedura
011726130905     c                   MOVEL     *blanks       d00op1                         *livello programma
011727130905     c                   MOVEL     '0'           d00f03                         *premuto F03
011728130905     c                   MOVEL     '0'           d00f12                         *premuto F12
011729130905     c                   MOVEL     '0'           d00err                         *errore
011730130905     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
011731130905     c                   CALL      'TISI46R'                                    *gestione versioni
011732130905     c                   PARM                    kpjba
011733130905     c                   PARM                    dssi00
011734130905     C* ritorno
011735130905if  1c                   IF        d00err='1'
011736130905     C                   SETON                                        7290
011737130905     C                   MOVEL     msg(1)        v1dmsg
011738130905x   1c                   ELSE
011739130910     c                   if        d00dde>0
011740130905     c                   Z-ADD     d00dde        g08inv
011741130905     c                   Z-ADD     *zeros        g08dat
011742130905     c                   MOVEL     '3'           g08err
011743130905     c                   CALL      'XSRDA8'
011744130905     c                   PARM                    wlbda8
011745130905     c                   Z-ADD     g08dat        v1cdri
011746130910     c                   endif
011747130905e   1c                   ENDIF
011748130905      /free
011749130905       endsr;
011750130905       //--------------------------------------------------------------
011751130905       //?Controlli videata
011752130905       //--------------------------------------------------------------
011753130905       BEGSR Ctrd01;
011754130905       *in70=*off;
011755130905       *in71=*off;
011756130905       *in72=*off;
011757130905       *in74=*off;
011758130905         // controllo data di riferimento
011759130905       // Controllo correttezza formale della data
011760130905       if %subst(%editc(v1cdri:'X'):1:2)='00';
011761130905          test(DE) *dmy v1cdri;
011762130905          if %error;
011763130905             *in90=*on;
011764130905             *in72=*on;
011765130905             *in28=*on;
011766130906             v1dmsg=msg(2);
011767130905             leavesr;
011768130905          else;
011769130905             dataiso=%date(v1cdri:*dmy);
011770130905          endif;
011771130905       else;
011772130905          test(DE) *eur v1cdri;
011773130905          if %error;
011774130905             *in90=*on;
011775130905             *in72=*on;
011776130905             *in28=*on;
011777130906             v1dmsg=msg(2);
011778130905             leavesr;
011779130905          else;
011780130905             dataiso=%date(v1cdri:*eur);
011781130905          endif;
011782130905       endif;
011783130905          dataeur=dataiso;
011784130905          v1cdri=%dec(dataeur);
011785130905          v1cdriinv=%dec(dataiso);
011786130906         // reperimento della versione
011787130906          p96dri=v1cdriinv;
011788130906          clear p96ver;
011789130906          GetVersioneCappario(p96dri:p96ver);
011790130906          if p96ver=0;
011791130906             *in90=*on;
011792130906             *in72=*on;
011793130906             *in28=*on;
011794130906             v1dmsg=msg(1);
011795130906             leavesr;
011796130906          endif;
011797130905         // errore se non immesso ne' cap nè lna
011798130910       if v1cca1=*blanks and v1cca2=*blanks and (v1clna=*blanks or
011799130905           v1clna=*zeros);
011800130905          *in90=*on;
011801130905          *in28=*on;
011802130905          *in70=*on;
011803130905          v1dmsg=msg(3);
011804130905          leavesr;
011805130905       endif;
011806130905         // CAP DAL-AL: se dal anche al e viceversa
011807130905       if (v1cca1<>*blanks and v1cca2=*blanks) or
011808130905          (v1cca2<>*blanks and v1cca1=*blanks);
011809130905          *in90=*on;
011810130905          *in28=*on;
011811130905          *in70=*on;
011812130905          v1dmsg=msg(4);
011813130905          leavesr;
011814130905       endif;
011815130905         // DAL <= AL
011816130905       if v1cca1<>*blanks and v1cca1>v1cca2;
011817130905          *in90=*on;
011818130905          *in28=*on;
011819130905          *in70=*on;
011820130905          v1dmsg=msg(5);
011821130905          leavesr;
011822130905       endif;
011823130905        //Linea di arrivo
011824130905        clear v1dlna;
011825130905        if %scan('?':v1clna)>0;
011826130905           clear pinfil   ;
011827130905           clear pinfag   ;
011828130905           clear pindes   ;
011829130905           clear pindit   ;
011830130905           tnsd24r(pinfil:pinfag:pindes:pindit)   ;
011831130905           if pinfil > *zeros;
011832130905              v1clna=pinfil;
011833130905              v1dlna=pindes;
011834130905           else;
011835130905              clear v1clna;
011836130905           endif;
011837130905           *in90=*on;
011838130905           leavesr;
011839130905        endif;
011840130905        // solo numeri
011841130906       if    v1clna<>*blanks;
011842130905           if    %check( c_Digits : V1Clna ) > *zero;
011843130906             V1Dmsg = Msg(07);
011844130905             *in74  = *on;
011845130905             *in90  = *on;
011846130905             *in28  = *on;
011847130905             leavesr;
011848130905           endif;
011849130905        // controlla esistenza del codice
011850130906           k_ORGfil = %int( V1Clna );
011851130906           chain  %kds( k01azorg01 )  AZORG;
011852130905
011853130906           if  NOT %found(AZORG01L)  or  ORGfva <> *blank or orgfag='V';
011854130906              V1Dmsg = Msg(07);
011855130906              *in74  = *on;
011856130906              *in90  = *on;
011857130906              *in28  = *on;
011858130906              leavesr;
011859130906           endif;
011860130905
011861130906           V1Dlna = ORGdes;
011862130906       endif;
011863130905       endsr;
011864130905       //--------------------------------------------------------------
011865130905       //?Conferma stampa
011866130905       //--------------------------------------------------------------
011867130905       BEGSR Confe ;
011868130906       DO6DRI=v1cdri;
011869130906       DO6VER=p96ver;
011870130909       DO6nar=v1cnar;
011871130906       DO6CA1=v1cca1;
011872130906       DO6CA2=v1cca2;
011873130906       if v1clna>*zeros;
011874130906          DO6LNA=%dec(v1clna:3:0);
011875130906       else;
011876130906          clear do6lna;
011877130906       endif;
011878130906         // sottometto azione di stampa
011879130906       kcoaz='SIO6';
011880130906       kpjbu=tisio6ds;
011881130906       bch10 (kpjba);
011882130906
011883130905       endsr;
011884130905       //--------------------------------------------------------------
011885130905       //?Operazioni iniziali.
011886130905       //--------------------------------------------------------------
011887130905       BEGSR RoutInz;
011888130905
011889130905       $inzd01=*on;
011890130905         // Reperimento dati job
011891130905         exsr DatiJob;
011892130905       // Reperisco la data corrente
011893130905          datacur=(%date());
011894130905
011895130905         $Fine=*off   ;
011896130905
011897130905       endsr;
011898130905       //--------------------------------------------------------------
011899130905       //?Reperimento Dati del job (Utente/Operativi).
011900130905       //--------------------------------------------------------------
011901130905       BEGSR DatiJob;
011902130905
011903130905         in(E) §AzUte;
011904130905         if NOT %error;
011905130905           in(E) §DatiUte;
011906130905         endif;
011907130905         if %error or RSut = *blanks;
011908130905           clear TIBS34ds;
011909130905           tibs34r(tibs34ds);
011910130905           in §AzUte;
011911130905           in §DatiUte;
011912130905         endif;
011913130905
011914130905       ENDSR;
011915130905       //--------------------------------------------------------------
011916130905       //?Operazioni finali.
011917130905       //--------------------------------------------------------------
011918130905       BEGSR RoutEnd;
011919130905
011920130905         // Chiusura pgm   ;
011921130905
011922130905         *inLR = *on;
011923130905
011924130905         return;
011925130905
011926130905       ENDSR;
011927130905
120200970910** MSG - MESSAGGI
120201130906Non esistono dati nel periodo richiesto                                       1
120300130905Data mancante o errata                                                        2
120400130905Effettuare almeno una richiesta (Cap o Linea di Arrivo)                       3
120500130905Se richiesto il Dal immettere anche l'AL e viceversa                          4
120600130905Il Dal deve essere inferiore o uguale all'AL                                  5
120700130905Ammessi SOLO caratteri numerici                                               6
120800130906Filiale errata                                                                7
