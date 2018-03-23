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
001908130905      // ricerca filiale su organigramma
001909130905     d   pInFIL        S              3
001910130905     d   pInFAG        S              1
001911130905     d   pInDES        S             25
001912130905     d   pInDIT        S              3
001913130906      // ricerca versione cappario
001914130906     d p96dri          s              8  0
001915130906     d p96ver          s              5  0
001916130905
001917130905     d $Fine           s               n   inz(*off)
001918130905     d $InzD01         s               n   inz(*on)
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
011503130905      /free
011504130905
011505130905       // Operazioni iniziali
011506130905       exsr RoutInz;
011507130905
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
011679130905         // data riferimento - Propongo data del giorno
011680130905         v1cdri=%dec(datacur:*eur);
011681130909         // Visualizzo la nazione che per il momento è fissa Italia
011682130909         clear v1cnar;
011683130909         clear v1dnar;
011684130909         clear wkey;
011685130909         chain (1:'15':wkey) tabel00f;
011686130909         if %found(tabel00f) and tblflg=*blanks;
011687130909            ds15=tbluni;
011688130909            v1dnar=§15des;
011689130909         endif;
011690130905         clear v1cca1;
011691130905         clear v1cca2;
011692130905         clear v1clna;
011693130905         clear v1dlna;
011694130905       endsr;
011695130905       //--------------------------------------------------------------
011696130905       //?F10=Ricerca Versioni
011697130905       //--------------------------------------------------------------
011698130905       BEGSR f10ges;
011699130905      /end-free
011700130905     c                   setoff                                       72
011701130905     C* lancia il programma di gestione versioni in ricerca
011702130905     c                   CLEAR                   dssi00                         *pulisce ds
011703130905     c                   MOVEL     'R01'         d00op0                         *livello procedura
011704130905     c                   MOVEL     *blanks       d00op1                         *livello programma
011705130905     c                   MOVEL     '0'           d00f03                         *premuto F03
011706130905     c                   MOVEL     '0'           d00f12                         *premuto F12
011707130905     c                   MOVEL     '0'           d00err                         *errore
011708130905     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
011709130905     c                   CALL      'TISI46R'                                    *gestione versioni
011710130905     c                   PARM                    kpjba
011711130905     c                   PARM                    dssi00
011712130905     C* ritorno
011713130905if  1c                   IF        d00err='1'
011714130905     C                   SETON                                        7290
011715130905     C                   MOVEL     msg(1)        v1dmsg
011716130905x   1c                   ELSE
011717130910     c                   if        d00dde>0
011718130905     c                   Z-ADD     d00dde        g08inv
011719130905     c                   Z-ADD     *zeros        g08dat
011720130905     c                   MOVEL     '3'           g08err
011721130905     c                   CALL      'XSRDA8'
011722130905     c                   PARM                    wlbda8
011723130905     c                   Z-ADD     g08dat        v1cdri
011724130910     c                   endif
011725130905e   1c                   ENDIF
011726130905      /free
011727130905       endsr;
011728130905       //--------------------------------------------------------------
011729130905       //?Controlli videata
011730130905       //--------------------------------------------------------------
011731130905       BEGSR Ctrd01;
011732130905       *in70=*off;
011733130905       *in71=*off;
011734130905       *in72=*off;
011735130905       *in74=*off;
011736130905         // controllo data di riferimento
011743130905       // Controllo correttezza formale della data
011744130905       if %subst(%editc(v1cdri:'X'):1:2)='00';
011745130905          test(DE) *dmy v1cdri;
011746130905          if %error;
011747130905             *in90=*on;
011748130905             *in72=*on;
011749130905             *in28=*on;
011750130906             v1dmsg=msg(2);
011751130905             leavesr;
011752130905          else;
011753130905             dataiso=%date(v1cdri:*dmy);
011754130905          endif;
011755130905       else;
011756130905          test(DE) *eur v1cdri;
011757130905          if %error;
011758130905             *in90=*on;
011759130905             *in72=*on;
011760130905             *in28=*on;
011761130906             v1dmsg=msg(2);
011762130905             leavesr;
011763130905          else;
011764130905             dataiso=%date(v1cdri:*eur);
011765130905          endif;
011766130905       endif;
011767130905          dataeur=dataiso;
011768130905          v1cdri=%dec(dataeur);
011769130905          v1cdriinv=%dec(dataiso);
011770130906         // reperimento della versione
011771130906          p96dri=v1cdriinv;
011772130906          clear p96ver;
011773130906          GetVersioneCappario(p96dri:p96ver);
011777130906          if p96ver=0;
011778130906             *in90=*on;
011779130906             *in72=*on;
011780130906             *in28=*on;
011781130906             v1dmsg=msg(1);
011782130906             leavesr;
011784130906          endif;
011785130905         // errore se non immesso ne' cap nè lna
011786130910       if v1cca1=*blanks and v1cca2=*blanks and (v1clna=*blanks or
011787130905           v1clna=*zeros);
011788130905          *in90=*on;
011789130905          *in28=*on;
011790130905          *in70=*on;
011791130905          v1dmsg=msg(3);
011792130905          leavesr;
011793130905       endif;
011794130905         // CAP DAL-AL: se dal anche al e viceversa
011795130905       if (v1cca1<>*blanks and v1cca2=*blanks) or
011796130905          (v1cca2<>*blanks and v1cca1=*blanks);
011797130905          *in90=*on;
011798130905          *in28=*on;
011799130905          *in70=*on;
011800130905          v1dmsg=msg(4);
011801130905          leavesr;
011802130905       endif;
011803130905         // DAL <= AL
011804130905       if v1cca1<>*blanks and v1cca1>v1cca2;
011805130905          *in90=*on;
011806130905          *in28=*on;
011807130905          *in70=*on;
011808130905          v1dmsg=msg(5);
011809130905          leavesr;
011810130905       endif;
011811130905        //Linea di arrivo
011812130905        clear v1dlna;
011813130905        if %scan('?':v1clna)>0;
011814130905           clear pinfil   ;
011815130905           clear pinfag   ;
011816130905           clear pindes   ;
011817130905           clear pindit   ;
011818130905           tnsd24r(pinfil:pinfag:pindes:pindit)   ;
011819130905           if pinfil > *zeros;
011820130905              v1clna=pinfil;
011821130905              v1dlna=pindes;
011822130905           else;
011823130905              clear v1clna;
011824130905           endif;
011825130905           *in90=*on;
011826130905           leavesr;
011827130905        endif;
011828130905        // solo numeri
011829130906       if    v1clna<>*blanks;
011830130905           if    %check( c_Digits : V1Clna ) > *zero;
011831130906             V1Dmsg = Msg(07);
011832130905             *in74  = *on;
011833130905             *in90  = *on;
011834130905             *in28  = *on;
011835130905             leavesr;
011836130905           endif;
011837130905        // controlla esistenza del codice
011838130906           k_ORGfil = %int( V1Clna );
011839130906           chain  %kds( k01azorg01 )  AZORG;
011840130905
011841130906           if  NOT %found(AZORG01L)  or  ORGfva <> *blank or orgfag='V';
011842130906              V1Dmsg = Msg(07);
011843130906              *in74  = *on;
011844130906              *in90  = *on;
011845130906              *in28  = *on;
011846130906              leavesr;
011847130906           endif;
011848130905
011849130906           V1Dlna = ORGdes;
011850130906       endif;
011851130905       endsr;
011852130905       //--------------------------------------------------------------
011853130905       //?Conferma stampa
011854130905       //--------------------------------------------------------------
011855130905       BEGSR Confe ;
011856130906       DO6DRI=v1cdri;
011857130906       DO6VER=p96ver;
011858130909       DO6nar=v1cnar;
011859130906       DO6CA1=v1cca1;
011860130906       DO6CA2=v1cca2;
011861130906       if v1clna>*zeros;
011862130906          DO6LNA=%dec(v1clna:3:0);
011863130906       else;
011864130906          clear do6lna;
011865130906       endif;
011866130906         // sottometto azione di stampa
011867130906       kcoaz='SIO6';
011868130906       kpjbu=tisio6ds;
011869130906       bch10 (kpjba);
011870130906
011871130905       endsr;
011872130905       //--------------------------------------------------------------
011873130905       //?Operazioni iniziali.
011874130905       //--------------------------------------------------------------
011875130905       BEGSR RoutInz;
011876130905
011877130905       $inzd01=*on;
011878130905         // Reperimento dati job
011879130905         exsr DatiJob;
011880130905       // Reperisco la data corrente
011881130905          datacur=(%date());
011882130905
011883130905         $Fine=*off   ;
011884130905
011885130905       endsr;
011886130905       //--------------------------------------------------------------
011887130905       //?Reperimento Dati del job (Utente/Operativi).
011888130905       //--------------------------------------------------------------
011889130905       BEGSR DatiJob;
011890130905
011891130905         in(E) §AzUte;
011892130905         if NOT %error;
011893130905           in(E) §DatiUte;
011894130905         endif;
011895130905         if %error or RSut = *blanks;
011896130905           clear TIBS34ds;
011897130905           tibs34r(tibs34ds);
011898130905           in §AzUte;
011899130905           in §DatiUte;
011900130905         endif;
011901130905
011902130905       ENDSR;
011903130905       //--------------------------------------------------------------
011904130905       //?Operazioni finali.
011905130905       //--------------------------------------------------------------
011906130905       BEGSR RoutEnd;
011907130905
011908130905         // Chiusura pgm   ;
011909130905
011910130905         *inLR = *on;
011911130905
011912130905         return;
011913130905
011914130905       ENDSR;
011915130905
120200970910** MSG - MESSAGGI
120201130906Non esistono dati nel periodo richiesto                                       1
120300130905Data mancante o errata                                                        2
120400130905Effettuare almeno una richiesta (Cap o Linea di Arrivo)                       3
120500130905Se richiesto il Dal immettere anche l'AL e viceversa                          4
120600130905Il Dal deve essere inferiore o uguale all'AL                                  5
120700130905Ammessi SOLO caratteri numerici                                               6
120800130906Filiale errata                                                                7
