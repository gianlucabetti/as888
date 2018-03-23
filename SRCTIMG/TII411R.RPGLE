000100170321     H DFTACTGRP(*NO) BNDDIR('PJXBND':'YDR') ACTGRP('QILE')
000200860421     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400850705     F*----------*                                                   *
000500171019     F*              - IMMISSIONE stime
000600850705     F*              --------------------------------                *
012200860528     D KPJBA         E DS
012201171019     d imofds00      e ds
013000990614      *
013500170321      *__________________________________________________________
013501171019     d wrkEml          s            253
013502171019     d wrkEmcc         s            253
013503171019     d wrkMsg          s           5000
013504171019     d wrkOgg          s             44
013505171019     d wrkesit         s              1
013516171019     d dataiso         s               d   datfmt(*iso)
013517171019     d dataeur         s               d   datfmt(*eur)
013600170321     D Skmessaggi      s            100a   DIM(255) varying
013800170321     D idrigaere       s             10I 0 inz
013900170321     D nrMessaggi      s             10I 0 inz
014000170321     D idMessaggio     s              7    dim(255)
014100170321     D tipoMessaggio   s              1    dim(255)
014200170321     D idcampo         s             10    dim(255)
014300170321     D testomessaggio  s            255a   DIM(255) varying
014600170321     d ydrereds      e ds                  extname(ydrere00f)
014700170321     d                                     QUALIFIED INZ(*EXTDFT)
014800170321     D*  Prototipazione delle procedure
014900170321      /COPY GAITRASRC/SRCCONST,YDRERE
015000170321      /COPY GAITRASRC/SRCPROTOPR,YDRERE
015100990614     D/COPY *LIBL/SRCCPY,PJX002PR
015200000000     C*----------------------------------------------------*
015300000000     C     *ENTRY        PLIST                                                  *
015400000000     C                   PARM                    KPJBA                          *
015500860421     C*                                                    *
015600171019     C                   movel     KPJBU         imofds00                       *
109700171019     C*
109800170321      /free
109900170321
110100170321       YDRere_Init(YDRERE_INSERIMENTO_AUTOMATICO : kpjba);
110400170321
110500171019       if idriga = 0 ;
110600170321        // inserimento
110700170321       esito=YDRere_NewDocumento(YDRERE_OPERAZIONE_INSERT);
110800170321       else;
110900170321        // aggiornamento
111100171019       esito=YDRere_NewDocumento(YDRERE_OPERAZIONE_UPDATE:idriga:*omit
111200170321        :'MAGAZZINO');
111300170321       endif;
111400170321
111500170321       esito=YDRere_SetAmbito('MAGAZZINO');
111600170321
113200170321       esito=YDRere_SetArticolo(articolo);
113300170321
113400171102       esito=YDRere_SetDataConsegna(%date(mofdbl));
113500170321
113600170321       esito=YDRere_SetDDT(%char(mofnbl):%date(mofdbl)); // facoltativo
113700170321
113701171019       // richiamo metodo per dtata e numero fattira solo se valorizzati
113702171019       if mofdft > 0 ;
113800171019         esito=YDRere_SetFAT(%char(mofnft):%date(mofdft)); // facoltativo
113900171019       endif;
113901171019
114000170321       esito=YDRere_SetFornitore(moffor:' ':' ');
114100170321
114300170321       esito=YDRere_SetImporto(prezzo:'EUR');
114400170321
114500170502       // if mofcdc <> *blank;
114600170502       //   esito=YDRere_SetEntita(mofcdc:'CDC');
114700170502       // endif;
114800170321
114900170428       esito=YDRere_SetOrdine(%char(mofnor):%date(mofdor));
115000170321
115100170321       esito=YDRere_SetNote(mofdmt); // descrizione materiale
115200170321
115300170321       esito=YDRere_SetIdFiliale(046); // facoltativo
115400170321
115500170321       esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
115600170321             : idCampo : testoMessaggio);
115700170321
115800170321       esito=YDRere_EndDocumento('1':'1');
115900170321       if esito > 0;
116000171019        idriga = esito;
116600170321       endif;
116601171019       kpjbu = imofds00 ;
116700171019       YDRere_finalize();
116800170321
116900170321      /end-free
117000171019     C                   seton                                        lr
