000100161202     Fazorg01L  uF   e           k disk
000101170201     Fazorg02L  iF   e           k disk    rename(azorg:azorg002)
000102170201     Ftabel00f  iF   e           k disk
000103170201     Ftnvrazor2dCF   E             WORKSTN
000200161202     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000201170201
000202170202     D MSG             S             78    DIM(7) CTDATA PERRCD(1)
000203170201
000204170201     D §KUT            s                   like(codut)
000205170201     D §COD            s                   like(TBLCOD)
000206170201     D §KEY            s                   like(TBLkey)
000207170201     d codut           s              1  0 Inz(1)
000208170201     d $Fine           s               n   inz(*off)
000209170201     d $InzD01         s               n   inz(*on)
000210170202     D WARE            s                   like(ORGCAR)
000211170616     d dataiso         s               d   datfmt(*iso)
000212170616     d datacur         s              8  0
000213170201
000214170201     d* Questo pgm aggiorna orario servizio clienti in base a distretto/area
000215170201     d* richiesti. Se non specificato distretto/area aggiorna tutto
000216170201
000300021118
000400170202     d kpjba         e ds
000401170707     d Og130         e ds                  inz
000402170620     d fnlv55ds      e ds                  inz
000403170201     d Tibs34ds      e ds                  inz
000404170201     d §azute        e ds                  Extname(Azute00f)
000405170201     d                                     dtaara
000406170201     d §Datiute      e ds                  EXTname(ddatiute)
000407170201     d                                     dtaara
000408170202      // ? PROTOTIPI ?
000409170202      /copy gaitrasrc/srcprotopr,tibs34r
000500021118
000600161202     c     *entry        plist
000700170201     c                   parm                    kpjba
000900161202     c
000901170201     C     Ktab          klist
000902170201     C                   kfld                    codut
000903170202     C                   kfld                    §cod
000904170202     C                   kfld                    §key
000905170201     c     kORG          klist
000906170201     c                   kfld                    v1cdst
000907170201     c                   kfld                    ware
000908170201     c     KORG1         klist
000909170201     c                   kfld                    orgfl3
000910170201     c                   kfld                    orgcar
000911170201      /free
000912170201       exsr datijob  ;
000913170201       //?Gestione video
000914170201       DOW $Fine = *off;
000915170201             exsr GesD01;
000916170201       ENDDO;
000917170201
000918170201       eval *inlr=*on;
000919170201       //-------------------------------------------------------------*
000920170201       //                                                             *
000921170201       //-------------------------------------------------------------*
000922170201       Begsr Gesd01;
000923170201       // Inizializzazione videata
000924170201          if $inzd01;
000925170201              exsr sr_inzd01;
000926170201              $InzD01  = *off;
000927170201          endif;
000928170201             write t01;
000929170201             exfmt d01;
000930170201             clear v1dmsg;
000931170201             *in28=*off;
000932170201             *in90=*off;
000933170201             select;
000934170201             // F3=Fine
000935170201                when *inkc;
000936170201                   $fine=*on;
000937170201                   leavesr;
000938170201                other;
000939170201                   exsr ctrd01;
000940170201                   if *in90;
000941170201                      leavesr;
000942170201                   endif;
000943170201                   if *inkf;
000944170201                      exsr sr_aggiOrg ;
000945170201                      $fine=*on;
000946170201                      leavesr;
000947170201                   endif;
000948170201             endsl;
000949170201       endsr;
000950170201       //-------------------------------------------------------------*
000951170201       // Inizializzazione prima videata                              *
000952170201       //-------------------------------------------------------------*
000953170201       Begsr sr_inzD01;
000954170202         clear v1cdst;
000955170202         clear v1ccar;
000956170202         clear v1ddst;
000957170202         clear v1dcar;
000958170201         clear v01oramd;
000959170201         clear v01orama;
000960170201         clear v01orapd;
000961170201         clear v01orapa;
000962170616         clear v01oramd2;
000963170616         clear v01orama2;
000964170616         clear v01orapd2;
000965170616         clear v01orapa2;
000966170201         clear v1dmsg;
000977170201         *in28=*off;
000978170201         *in40=*off;
000979170201         *in41=*off;
000980170201         *in42=*off;
000981170201         *in43=*off;
000982170201         *in44=*off;
000983170201         *in45=*off;
000984170616         *in46=*off;
000985170616         *in47=*off;
000986170616         *in48=*off;
000987170616         *in49=*off;
000988170201       endsr;
000989170201       //-------------------------------------------------------------*
000990170201       // Controllo prima videata                                     *
000991170201       //-------------------------------------------------------------*
000992170201       Begsr ctrd01  ;
000993170201
000994170201       *in40=*off;
000995170201       *in41=*off;
000996170201       *in42=*off;
000997170201       *in43=*off;
000998170201       *in44=*off;
000999170201       *in45=*off;
001000170616       *in46=*off;
001001170616       *in47=*off;
001002170616       *in48=*off;
001003170616       *in49=*off;
001004170201
001005170201       // distretto
001006170201       exsr CtrDistr ;
001007170201       if *in90 ;
001008170201          leavesr;
001009170201       endif;
001010170201       // area
001011170201       exsr CtrAREA;
001012170201       if *in90 ;
001013170201          leavesr;
001014170201       endif;
001015170201
001016170201       // Orari
001017170201       exsr CtrOrari;
001018170201       if *in90 ;
001019170201          leavesr;
001020170201       endif;
001021170201
001089170201
001090170201       endsr;
001091170201       //-------------------------------------------------------------*
001092170201       //Reperimento Dati del job (Utente/Operativi)                  *
001093170201       //-------------------------------------------------------------*
001094170201       Begsr DatiJob;
001095170201
001096170201          in(e) §azute;
001097170201          if not %error;
001098170201           in(e) §datiute;
001099170201          endif;
001100170201          if %error or rsut = *blanks;
001101170201           tibs34r(tibs34ds);
001102170201           in §azute;
001103170201           in §datiute;
001104170201          endif;
001106170616          dataiso=%date();
001107170616          datacur =%dec(dataiso);
001109170201
001110170201         endsr;
001111170201      /end-free
001112170201      *--------------------------------------------------------------------*
001113170201     c* Controllo codice distretto
001114170201      *--------------------------------------------------------------------*
001115170201     c     CtrDISTR      BEGSR
001116170201     c                   clear                   v1ddst
001117170201     c     '?'           scan      v1cdst                                 90
001118170201    1c                   if        *in90
001119170201     c                   movel     codut         §kut
001120170201     c                   movel     '17'          §cod
001121170201     c                   clear                   §key
001122170201     c                   call      'X§TABER'
001123170201     c                   parm                    §kut
001124170201     c                   parm                    §cod
001125170201     c                   parm                    §key
001126170201     c                   parm                    §des             25
001127170201     c                   movel     §key          v1cdst
001128170201     c                   movel     §des          v1ddst
001129170201     c                   seton                                        40
001130170201     c                   goto      endctrDISTR
001131170201    1c                   endif
001132170201     c
001133170201    1c     v1cdst        ifne      *blanks
001134170201     c
001135170202     c                   movel     '17'          §cod
001136170202     c                   movel(p)  v1cdst        §key
001137170201     c     ktab          chain     tabel                              30
001138170201    2c                   if        *in30 OR tblflg<>' '
001139170202     c                   movel     msg(1)        v1dmsg
001140170201     c                   seton                                        289040
001141170201     c                   goto      endctrDISTR
001142170201    2c                   endif
001143170201     c                   movel     tbluni        v1ddst
001144170201     C* Non è possibile richiedere il distretto '9' - SEDE
001145170201    2C                   if        v1cdst='9'
001146170202     c                   movel     msg(2)        v1dmsg
001147170201     c                   seton                                        289040
001148170201     c                   goto      endctrDISTR
001149170201    2c                   endif
001150170201     c
001151170201     c
001152170201     c* controllo se esiste in organigramma
001153170201     c     v1cdst        chain     azorg02l                           30
001154170201    2c                   dow       not *in30 and orgfva<>' '
001155170201     c     v1cdst        reade     azorg02l                               30
001156170201    2c                   enddo
001157170201    2c                   if        *in30
001158170202     c                   movel     msg(03)       v1dmsg
001159170201     c                   seton                                        289040
001160170201     c                   goto      endctrDISTR
001161170201    2c                   endif
001167170201    1c                   endif
001168170201     c     EndctrDISTR   ENDSR
001169170201      *--------------------------------------------------------------------*
001170170201     c* controllo cod AREA
001171170201      *--------------------------------------------------------------------*
001172170201     c     CtrAREA       BEGSR
001173170201     c* Ricerca
001174170201     c                   clear                   v1dcar
001175170201     c     '?'           scan      v1ccar                                 90
001176170201    1c                   if        *in90
001177170201     c                   movel     codut         §kut
001178170201     c                   movel     '05'          §cod
001179170201     c                   clear                   §key
001180170201     c                   call      'X§TABER'
001181170201     c                   parm                    §kut
001182170201     c                   parm                    §cod
001183170201     c                   parm                    §key
001184170201     c                   parm                    §des
001185170201     c                   movel     §key          v1ccar
001186170201     c                   movel     §des          v1dcar
001187170201     c                   seton                                        41
001188170201     c                   goto      endctrAREA
001189170201    1c                   endif
001190170201     c     v1ccar        ifeq      *zeros
001191170201     c                   clear                   v1ccar
001192170201     c                   endif
001193170201     c*
001194170201    1c     v1ccar        ifne      *blanks
001195170201     c     v1ccar        andne     *zeros
001196170201     c*
001197170201     c* Deve esistere in tabella aree
001198170202     c                   movel     '05'          §cod
001199170202     c                   movel(p)  v1ccar        §key
001200170201     c     ktab          chain     tabel                              30
001201170201    2c                   if        *in30 OR tblflg<>' '
001202170202     c                   movel     msg(04)       v1dmsg
001203170201     c                   seton                                        289041
001204170201     c                   goto      endctrAREA
001205170201    2c                   endif
001206170201     c                   movel     tbluni        v1dcar
001207170201     c
001208170201    2c     v1ccar        ifle      *zeros
001209170201     c                   clear                   ware
001210170201     c                   else
001211170201     c                   movel     v1ccar        ware
001212170201    2c                   endif
001213170201     c
001214170201     c* se immesso il distretto devono essere congruenti
001215170201    2c                   if        v1cdst<>' '
001216170201     c     KORG          chain     azorg02l                           30
001217170201    3c                   dow       not *in30 and orgfva<>' '
001218170201     c     KORG          reade     azorg02l                               30
001219170201    3c                   enddo
001220170201    3c                   if        *in30
001221170202     c                   movel     msg(05)       v1dmsg
001222170201     c                   seton                                        289041
001223170201     c                   goto      endctrAREA
001224170201    3c                   endif
001225170201    2c                   endif
001226170201     c
001236170201     c
001237170201     c* valorizzo distretto se non immesso
001238170201     c     *loval        setll     azorg02l
001239170201     c                   read      azorg02l                               30
001240170201    2c                   dow       not *in30  and v1cdst=*blanks
001241170201    3c                   if        orgcar<>ware
001242170201     c     korg1         setgt     azorg02l
001243170201   x3c                   else
001244170201    4c     orgfva        ifeq      *blanks
001245170201     c                   movel     orgfl3        v1cdst
001246170201    4c                   endif
001247170201    3c                   endif
001248170201     c                   read      azorg02l                               30
001249170201    2c                   enddo
001250170201     c
001251170201     C* Non è possibile richiedere il distretto '9' - SEDE
001252170201     C                   if        v1cdst='9'
001253170201     c                   clear                   v1cdst
001254170202     c                   movel     msg(06)       v1dmsg
001255170201     c                   seton                                        289041
001256170201     c                   goto      endctrAREA
001257170201     c                   endif
001258170201     c* non trovata area sull'organigramma
001259170201     C                   if        v1cdst=' '
001260170201     c                   clear                   v1cdst
001261170202     c                   movel     msg(07)       v1dmsg
001262170201     c                   seton                                        289041
001263170201     c                   goto      endctrAREA
001264170201     c                   endif
001265170201     c
001266170201     c                   exsr      decDISTR
001267170201     c*
001268170201     c                   endif
001269170201     c     endctrAREA    ENDSR
001270170201      *****************************************************************
001271170201     c* decodifica distretto
001272170201      *****************************************************************
001273170201     c     decDISTR      begsr
001274170201     c* Decodifico codice distretto
001275170202     c                   movel     '17'          §cod
001276170202     c                   movel(p)  v1cdst        §key
001277170201     c     ktab          chain     tabel                              30
001278170201     c  N30              movel     tbluni        v1ddst
001279170201     c                   endsr
001280170201      *****************************************************************
001281170201     c* Controllo orari
001282170201      *****************************************************************
001283170201     c     CtrOrari      begsr
001284170201      * Controllo che gli orari immessi siano validi
001285170202     c                   IF        V01oramd<1
001286170202     c                             or V01oramd>2359
001287170201     c                   eval      *in42 = *on
001288170201     c                   eval      *in28 = *on
001289170201     c                   eval      *in90 = *on
001290170201     c                   eval      V1dmsg = 'Orario errato'
001291170201     c                   leavesr
001292170201     c                   ENDIF
001293170201
001294170202     c                   IF        V01orama < 1
001295170202     c                             or V01orama > 2359
001296170201     c                   eval      *in43 = *on
001297170201     c                   eval      *in28 = *on
001298170201     c                   eval      *in90 = *on
001299170201     c                   eval      V1dmsg = 'Orario errato'
001300170201     c                   leavesr
001301170201     c                   ENDIF
001302170201      * l'ora dalle del mattino non può essere superiore dell'ora alle del mattino
001303170202     c                   if        v01oramd>v01orama
001304170201     c                   eval      *in43 = *on
001305170201     c                   eval      *in28 = *on
001306170201     c                   eval      *in90 = *on
001307170202     c                   if        v01orama=0
001308170201     c                   eval      V1dmsg = 'Orario errato'
001309170201     c                   else
001310170201     c                   eval      V1dmsg = 'Orario "dalle" non congruente con +
001311170201     c                             orario "alle"'
001312170201     c                   endif
001313170201     c                   leavesr
001314170201     c                   endif
001315170201
001316170201      * l'ora dalle del pomeriggio non può essere minore dell'ora alle del mattino
001317170202     c                   IF        V01orapd > 0 and V01orapd < V01orama
001318170201     c                   eval      *in44 = *on
001319170201     c                   eval      *in28 = *on
001320170201     c                   eval      *in90 = *on
001321170201     c                   eval      V1dmsg = 'Orario non congruente con quello +
001322170201     c                             del mattino'
001323170201     c                   leavesr
001324170201     c                   ENDIF
001325170201
001326170202     c                   IF        V01orapd > 0
001327170202     c                   IF        V01orapd < 1 or V01orapd > 2359
001328170201     c                   eval      *in44 = *on
001329170201     c                   eval      *in28 = *on
001330170201     c                   eval      *in90 = *on
001331170201     c                   eval      V1dmsg = 'Orario errato'
001332170201     c                   leavesr
001333170201     c                   ENDIF
001334170201     c                   ENDIF
001335170201
001336170202     c                   IF        V01orapd > 0 and V01orapa = 0
001337170201     c                   eval      *in45 = *on
001338170201     c                   eval      *in28 = *on
001339170201     c                   eval      *in90 = *on
001340170201     c                   eval      V1dmsg = 'Orario errato'
001341170201     c                   leavesr
001342170201     c                   ENDIF
001343170202     c                   IF        V01orapa > 0
001344170202     c                   IF        V01orapa < 1 or V01orapa > 2359
001345170201     c                   eval      *in45 = *on
001346170201     c                   eval      *in28 = *on
001347170201     c                   eval      *in90 = *on
001348170201     c                   eval      V1dmsg = 'Orario errato'
001349170201     c                   leavesr
001350170201     c                   ENDIF
001351170201     c                   ENDIF
001352170201      * l'ora dalle del pomeriggio non può essere superiore dell'ora alle del pom
001353170202     c                   if        v01orapd>v01orapa
001354170201     c                   eval      *in45 = *on
001355170201     c                   eval      *in28 = *on
001356170201     c                   eval      *in90 = *on
001357170202     c                   if        v01orapa=0
001358170201     c                   eval      V1dmsg = 'Orario errato'
001359170201     c                   else
001360170201     c                   eval      V1dmsg = 'Orario "dalle" non congruente con +
001361170201     c                             orario "alle"'
001362170201     c                   endif
001363170201     c                   leavesr
001364170201     c                   endif
001365170616      *************************************
001366170616      * Eventuali orari per secondi livello
001367170616      **************************************
001368170616     c                   if        v01oramd2>0
001369170616      * Controllo che gli orari immessi siano validi
001370170616     c                   IF        V01oramd2<1
001371170616     c                             or V01oramd2>2359
001372170616     c                   eval      *in46 = *on
001373170616     c                   eval      *in28 = *on
001374170616     c                   eval      *in90 = *on
001375170616     c                   eval      V1dmsg = 'Orario errato'
001376170616     c                   leavesr
001377170616     c                   ENDIF
001378170616
001379170616     c                   IF        V01orama2 < 1
001380170616     c                             or V01orama2 > 2359
001381170616     c                   eval      *in47 = *on
001382170616     c                   eval      *in28 = *on
001383170616     c                   eval      *in90 = *on
001384170616     c                   eval      V1dmsg = 'Orario errato'
001385170616     c                   leavesr
001386170616     c                   ENDIF
001387170616      * l'ora dalle del mattino non può essere superiore dell'ora alle del mattino
001388170616     c                   if        v01oramd2>v01orama2
001389170616     c                   eval      *in47 = *on
001390170616     c                   eval      *in28 = *on
001391170616     c                   eval      *in90 = *on
001392170616     c                   if        v01orama2=0
001393170616     c                   eval      V1dmsg = 'Orario errato'
001394170616     c                   else
001395170616     c                   eval      V1dmsg = 'Orario "dalle" non congruente con +
001396170616     c                             orario "alle"'
001397170616     c                   endif
001398170616     c                   leavesr
001399170616     c                   endif
001400170616
001401170616      * l'ora dalle del pomeriggio non può essere minore dell'ora alle del mattino
001402170616     c                   IF        V01orapd2 > 0 and V01orapd2 < V01orama2
001403170616     c                   eval      *in48 = *on
001404170616     c                   eval      *in28 = *on
001405170616     c                   eval      *in90 = *on
001406170616     c                   eval      V1dmsg = 'Orario non congruente con quello +
001407170616     c                             del mattino'
001408170616     c                   leavesr
001409170616     c                   ENDIF
001410170616
001411170616     c                   IF        V01orapd2 > 0
001412170616     c                   IF        V01orapd2 < 1 or V01orapd2 > 2359
001413170616     c                   eval      *in48 = *on
001414170616     c                   eval      *in28 = *on
001415170616     c                   eval      *in90 = *on
001416170616     c                   eval      V1dmsg = 'Orario errato'
001417170616     c                   leavesr
001418170616     c                   ENDIF
001419170616     c                   ENDIF
001420170616
001421170616     c                   IF        V01orapd2 > 0 and V01orapa2 = 0
001422170616     c                   eval      *in49 = *on
001423170616     c                   eval      *in28 = *on
001424170616     c                   eval      *in90 = *on
001425170616     c                   eval      V1dmsg = 'Orario errato'
001426170616     c                   leavesr
001427170616     c                   ENDIF
001428170616     c                   IF        V01orapa2 > 0
001429170616     c                   IF        V01orapa2 < 1 or V01orapa2 > 2359
001430170616     c                   eval      *in49 = *on
001431170616     c                   eval      *in28 = *on
001432170616     c                   eval      *in90 = *on
001433170616     c                   eval      V1dmsg = 'Orario errato'
001434170616     c                   leavesr
001435170616     c                   ENDIF
001436170616     c                   ENDIF
001437170616      * l'ora dalle del pomeriggio non può essere superiore dell'ora alle del pom
001438170616     c                   if        v01orapd2>v01orapa2
001439170616     c                   eval      *in49 = *on
001440170616     c                   eval      *in28 = *on
001441170616     c                   eval      *in90 = *on
001442170616     c                   if        v01orapa=0
001443170616     c                   eval      V1dmsg = 'Orario errato'
001444170616     c                   else
001445170616     c                   eval      V1dmsg = 'Orario "dalle" non congruente con +
001446170616     c                             orario "alle"'
001447170616     c                   endif
001448170616     c                   leavesr
001449170616     c                   endif
001450170616     c                   endif
001451170201     c                   endsr
001452170201
001453170201      *****************************************************************
001454170201     c* Aggiornamento organigramma
001455170201      *****************************************************************
001456170201     c     sr_aggiOrg    begsr
001457161202
001458161202     C                   CLEAR                   TIPOag           10
001459170202     c                   clear                   ware
001460170202     c                   if        v1ccar>*zeros
001461170202     c                   move      v1ccar        ware
001462170202     c                   endif
002000161202
002100170202     c                   if        %subst(kpjbu:1:3)='AGG'
002200161202     c                   eval      tipoag='- AGGIORNA'
002300161202     C                   ELSE
002400161202     c                   eval      tipoag='- SIMULA  '
002500161202     C                   ENDIF
002600161202     C
002700161202     C                   EXCEPT    TESTA
002800161202
002900161202     c     *loval        setll     azorg01l
003000161202     c                   read      azorg01l
003100161202     c
003200161202    2c                   dow       not %eof(azorg01l)
003300161202    3c                   if        orgfag='F' or orgfag='A'
003400161202     c* Escludo estero
003600161202     c
003700161202     c                   if        orgfl1<>'E'
003702170202
003703170202     c                   if        v1cdst<>*blanks and v1cdst<> orgfl3
003704170202     c                   read      azorg01l
003705170202     c                   iter
003706170202     c                   endif
003707170202
003708170202     c                   if        ware>0   and ware<>orgcar
003709170202     c                   read      azorg01l
003710170202     c                   iter
003711170202     c                   endif
003712170202
003800161202     c
003900170201     c                   eval      OG130 = orgdd0
003901170202     c* Se la filiale non ha orari la ignoro
003902170202     c                   if        (§OGoramd = *zeros or §OGoramd=*blanks) and
003903170202     c                             (§OGorama = *zeros or §OGorama=*blanks) and
003904170202     c                             (§OGorapd = *zeros or §OGorapd=*blanks) and
003905170202     c                             (§OGorapa = *zeros or §OGorapa=*blanks)
003908170202     c                   read      azorg01l
003909170202     c                   iter
003910170202     c                   endif
003911170616     c* Se specificati orari differenti fra primo e secondo livello
003912170616     c                   if        v01oramd2>0
003913170616     c* verifico se la filiale è un primo o un secondo livello
003914170616     C                   clear                   fnlv55ds
003915170620     C                   movel     'A'           d55tpt
003916170616     C                   z-add     orgfil        d55lin
003917170616     C                   z-add     datacur       d55drf
003918170616     C                   call      'FNLV55R'
003919170616     C                   PARM                    fnlv55ds
003920170620     c                   if        d55err = *blanks and d55tfa<>orgfil
003921170616     c* secondo livello
003922170616     c                   seton                                        50
003923170616     c                   else
003924170616     c* primo   livello
003925170616     c                   setoff                                       50
003926170616     c                   endif
003927170616     c                   endif
003928170202
003929170202     c                   except    det
004200161202
004900161202
005100161202     c
005200161202     c* Aggazorg
005301170202     c                   if        %subst(kpjbu:1:3)='AGG'
005302170616     c                   if        *in50
005303170616     c                   eval      §OGoramd = %editc(V01oramd2:'X')
005304170616     c                   eval      §OGorama = %editc(V01orama2:'X')
005305170616     c                   eval      §OGorapd = %editc(V01orapd2:'X')
005306170616     c                   eval      §OGorapa = %editc(V01orapa2:'X')
005307170616     c                   else
005308170202     c                   eval      §OGoramd = %editc(V01oramd:'X')
005309170202     c                   eval      §OGorama = %editc(V01orama:'X')
005310170202     c                   eval      §OGorapd = %editc(V01orapd:'X')
005311170202     c                   eval      §OGorapa = %editc(V01orapa:'X')
005312170616     c                   endif
005313170202     c                   eval      ORGdd0 = OG130
005400161202     c                   clear                   ORGFTR
005700161202     c                   except    aggazorg
005800161202     c                   endif
005900161202     c
006000161202     c
006400161202    3c                   ENDIF
006500161202    3c                   ENDIF
006600161202
006700161202     c                   read      azorg01l
006800161202    2c                   enddo
006801170201     c                   endsr
006900161202
007400021118      *________________________________________________
007500021118     oAZORG     e            aggAZORG
007600170201     o                       ORGDd0
007700161202     o                       ORGFTR
008200161202     OQSYSPRT   E            det         1
008300161202     O                                              'Fil'
008400161202     o                       orgfil           +   1
008500161202     o                       orgdes           +   1
008501170202     o                                        +   1 'Da:'
008600170201     o                       §ogoramd         +   1
008601170201     o                       §ogorama         +   1
008602170201     o                       §ogorapd         +   1
008603170201     o                       §ogorapa         +   1
008604170202     o
008605170202     o                                        +   1 'A :'
008606170616     o              n50      v01oramd         +   1
008607170616     o              n50      v01orama         +   1
008608170616     o              n50      v01orapd         +   1
008609170616     o              n50      v01orapa         +   1
008610170616     o               50      v01oramd2        +   1
008611170616     o               50      v01orama2        +   1
008612170616     o               50      v01orapd2        +   1
008613170616     o               50      v01orapa2        +   1
008700161202     OQSYSPRT   E            testa       1
008800170202     O                                              'Aggiorna orari Serv.Cli.'
009000161202     o                       tipoag           +   3
009001170201** MSG                                                                       *
009100170201Cod.Distretto errato                                                          01
009200170201Cod.Distretto 9 - SEDE  non selezionabile                                     02
009300170201Cod.Distretto non presente nell'organigramma                                  03
009400170202Cod.Area errato                                                               04
009500170202Cod.Area non congruente col distretto selezionato                             05
009700170202Cod.Area di distretto 9 - SEDE  non selezionabile                             06
009800170202Cod.Area non presente nell'organigramma                                       07
