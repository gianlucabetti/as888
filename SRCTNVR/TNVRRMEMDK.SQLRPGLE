000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000406111013     fwfemd00f  o  a E             disk
000407111013     fazorg01l  if   e           k disk
000408111012
000700090715     d
000701111013     d tnvrrmemds    e ds
000702111013     d titasds       E DS                  extname(titas00f)
000801111012     d dtasflo       e ds
000802111013     d TIBS69DS      E DS                  INZ
000803111013     d DS_cnaco      E DS                  extname(CNACO00F) prefix(ds_)
000804111013     d DS_cnind      E DS                  extname(CNIND00F) prefix(ds_)
000805111013     d DS_cnclp      E DS                  extname(CNCLP00F) prefix(ds_)
000806111013     d DS_fncls      E DS                  extname(FNCLS00F) prefix(ds_)
000807111013     d kfil            s                   like(orgfil)
000808111013     d WtotE           s              5  0
000809111013     d preccm          s                   like(tasccm)
000810111013     d dataiso_cor     s               d   datfmt(*iso)
000903111012     d num             s              3  0 dim(100)
000904111012     d numf            s              3  0 dim(100)
000905111012     d ccm             s              7  0 dim(100)
000912111012
000913111013     D WrkStringaSql   S           4500
000914111013     D                                     VARYING
001201111013     d $Finerec        s              1    inz(*off)
001202111013
001203111013      /copy gaitrasrc/srcprotopr,tibs69r
001204111013     c     *Entry        plist
001205111013     c                   parm                    KPJBA
001206111013     c
001207111013     c                   movel     kpjbu         tnvrrmemds
001208111013      /free
001209111013         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
001210111013
001211111013       // Reperisco la data corrente
001212111013       dataiso_cor=(%date());
001213111013
001214111013       WrkStringaSql='select * from titas00f where'               +
001215111013                     ' substr(tasflo, 9, 1)=''S'' '               +
001216111013                     ' and (tasaas*10000)+tasmgs between'         +
001217111013                     ' demddal and demdal'                        +
001218111013                     'union'                                      +
001219111013                     'select * from titas10f where'               +
001220111013                     ' substr(tasflo, 9, 1)=''S'' '               +
001221111013                     ' and (tasaas*10000)+tasmgs between'         +
001222111013                     ' demddal and demdal'                        +
001223111013                     'union'                                      +
001224111013                     'select * from titasp0f where'               +
001225111013                     ' substr(tasflo, 9, 1)=''S'' '               +
001226111013                     ' and (tasaas*10000)+tasmgs between'         +
001227111013                     ' demddal and demdal'                        +
001228111013                     'order by tasccm             '               ;
001229111013        $finerec=*off;
001230111013        exec sql prepare STRINGASQL from :WrkStringaSql;
001231111013        exec sql declare TASCsr cursor for StringaSql;
001232111013        exec sql open TAScsr;
001233111013        dow $finerec=*off;
001234111013           exec sql Fetch TAScsr into :titasds                    ;
001235111013           if sqlcod=100 or sqlcod<0;
001236111013              $finerec = *on;
001237111013              leave;
001238111013           endif;
001239111013        // a cambio di CCM azzero il contatore ed eseguo tutte
001240111013        // le operazioni di ricerca dati accessori e scrivo record nel
001241111013        // file di work
001242111013           if tasccm<>preccm              ;
001243111013              if preccm>0                 ;
001244111013                 exsr sr_aggWf            ;
001245111013              endif                       ;
001246111013              WTotE=0                     ;
001247111013              preccm=tasccm               ;
001248111013           endif                          ;
001249111013        // incremento contatore numero e-mail per ccm
001250111013           wtotE=+1  ;
001251111013        enddo;
001252111013       eval *inlr=*on;
001253111013       //-------------------------------------------------------------*
001254111013       Begsr sr_aggWf  ;
001255111013       clear wfemd000;
001256111013        emddel=%dec(dataiso_cor);
001257111013        emdUSR=knmus;
001258111013        EMDDAL=demddal;
001259111013        EMDAL=demdal;
001260111013        emdccm=preccm;
001261111013       // reperisco decodifica e commeciale del cliente
001262111013           clear DS_cnaco;
001263111013           clear DS_cnind;
001264111013           clear DS_cnclp;
001265111013           clear DS_fncls;
001266111013           I69kcp=preccm ;
001267111013           I69kac=preccm ;
001268111013           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
001269111013           emdrag=acorag;
001270111013           emdage=clpage;
001271111013       // reperisco area del commerciale e sua decodifica
001273111013           kfil=%dec(%subst(%editc(clpage:'X'):1:3):3:0);
001274111013           chain (kfil) azorg01l;
001275111013           if %found(azorg01l);
001276111013              emdcar=orgcar ;
001277111013           endif;
001279111013       //                  EMDDAR
001280111013       //                  EMDDAG
001281111013      //                   EMDCBA
001282111013           EMDTSP=Wtote;
001283111013       endsr;
