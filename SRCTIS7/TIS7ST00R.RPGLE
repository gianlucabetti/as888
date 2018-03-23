000100051121     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200120301     H* tis7st00r *--------------------------------------------------*
000300120301     H*              RICERCA tariffe cliente
000400000000     H*--------------------------------------------------------------*
000500940720     FTNTAM01L  IF   E           K DISK
000600941103     FTABEL00F  IF   E           K DISK
000700120305     Ftntbe11l  IF   E           K DISK
000800051121      *------------------------------------------------------------------------*
000900120301     d codT            s              3    dim(99)
001000120301     d desT            s             15    dim(99)
001100120301     d desNtw          s             12    dim(99)
001200120305     d ntw             s              3    dim(99)
001300120301     d desTSP          s             15    dim(99)
001400120301     d
001500090608     d ds5e          e ds
001600991022     D DSTA01        E DS
001700120301     d
001800120302     D tis7st00i     E DS                  prefix(i00_) INZ(*EXTDFT)
001900120302     D tis7st00o     E DS                  prefix(o00_) INZ(*EXTDFT)
002000120301     d
002100120302     D tis7st01i     E DS                  prefix(i01_) INZ(*EXTDFT)
002200120302     D tis7st01o     E DS                  prefix(o01_) INZ(*EXTDFT)
002300120302
002400120301     D rqsOpCode       S             10A
002500120301     D rqsData         S           9999A
002600120301     D rqsDataLen      S              4P 0
002700120301     D rpyOpCode       S             10A
002800120301     D rpyData         S           9999A
002900120301     D rpyDataLen      S              4P 0
003000120301     d
003100120301     d Datasys         s               d   datfmt(*iso) inz(*sys)
003200120301     d Dataiso         s               d   datfmt(*iso)
003300120301     d Dataeur         s               d   datfmt(*eur)
003400120301     d Datarif         s              8  0
003500120301     d codut           s              1  0
003600120301     d XX              s              3  0
003700120301     d werror          s              1  0
003800120305     d klin            s              1  0
003900120627     d savctr          s              3
004000120305     d LINGUA          s                   like(I00_LINISO2)
004100120301     ***************************************************************************
004200120301     ** Costanti.
004300120301     ***************************************************************************
004400120301     D ESITO_ERROR...
004500120301     D                 C                   -1
004600120301     D ESITO_OK...
004700120301     D                 C                   0
004800120301     D QTEMP...
004900120301     D                 C                   'QTEMP'
005000120301     D RPYOPCODE_DONE...
005100120301     D                 C                   'DONE'
005200120301     D RPYOPCODE_ERROR...
005300120301     D                 C                   'ERROR'
005400120301     D RQSOPCODE_GETTAMLIST...
005500120301     D                 C                   'GETTAMLIST'
005600120301     D RQSOPCODE_GETTAMDETT...
005700120301     D                 C                   'GETTAMDETT'
005800120301
005900000000     C*---------------------------------------------------------------*
006000000000     C     *ENTRY        PLIST
006100120301     C                   PARM                    rqsOpCode
006200120301     C                   PARM                    rqsData
006300120301     C                   PARM                    rqsDataLen
006400120301     C                   PARM                    rpyOpCode
006500120301     C                   PARM                    rpyData
006600120301     C                   PARM                    rpyDataLen
006700120301     c
006800120301     c                   clear                   rpyData
006900120301     c                   clear                   rpyDataLen
007000120301     c
007100120302     c                   eval      rpyopcode=RPYOPCODE_ERROR
007200120302
007300120301     c                   exsr      Contrdati
007400940720     C*
007500120302    1c                   if        Werror= ESITO_OK  and
007600120302 1   c                             rqsOpCode = RQSOPCODE_GETTAMLIST
007700120301     c                   exsr      ElabTar
007800120301    1c                   endif
007900120302     c
008000120302    1c                   if        Werror= ESITO_OK  and
008100120302 1   c                             rqsOpCode = RQSOPCODE_GETTAMDETT
008200120302     c                   exsr      SingolaTar
008300120302    1c                   endif
008400120302
008500120302    1c                   if        Werror= ESITO_OK
008600120302     c                   eval      rpyopcode=RPYOPCODE_DONE
008700120302
008800120302 1   c                   if        rqsOpCode = RQSOPCODE_GETTAMLIST
008900120302     c                   eval      rpyData = tis7st00o
009000120302     c                   eval      rpyDataLen = %SIZE(tis7st00o)
009100120302     c                   else
009200120302     c                   eval      rpyData = tis7st01o
009300120302     c                   eval      rpyDataLen = %SIZE(tis7st01o)
009400120302     c                   endif
009500120302     c
009600120302     c                   else
009700120320     c****               dump(a)
009800120314     c                   eval      rpyDataLen = %len(%trim(rpydata))
009900120302    1c                   endif
010000900323     C*
010100120302     C                   return
010200120301     c*------------------------------------------------------------------*
010300120301     C*   controlli dati passati
010400120301     c*------------------------------------------------------------------*
010500120301     c     ContrDATI     BEGSR
010600120301     c                   clear                   Werror
010700120301     c                   clear                   codT
010800120301     c                   clear                   desT
010900120305     c                   clear                   NTW
011000120305     c                   clear                   desNTW
011100120301     c                   clear                   desTSP
011200120301      /FREE
011300120301 1     IF rqsOpCode <> RQSOPCODE_GETTAMLIST and
011400120301          rqsOpCode <> RQSOPCODE_GETTAMDETT;
011500120301         WError=  ESITO_ERROR;
011600120306         rpyData = 'rqsOpCode errato o non gestito' ;
011700120830         //dump(a)  ;
011800120320         leavesr  ;
011900120301 1     ENDIF;
012000120301
012100120301
012200120302       // L I S T A     T A R I F F E
012300120301
012400120301 1     IF rqsOpCode = RQSOPCODE_GETTAMLIST ;
012500120302       reset tis7st00i;
012600120301
012700120301 2     SELECT;
012800120301         WHEN %SUBST(rqsData : 1 : 10) = i00_formato;
012900120301           tis7st00i = %SUBST(rqsData : 1 : rqsDataLen);
013000120301         OTHER;
013100120301           Werror= ESITO_ERROR;
013200120306         rpyData = 'Richiamo errato ' ;
013300120830         //dump(a)  ;
013400120320         leavesr  ;
013500120301 2     ENDSL;
013600120301
013700120302       monitor  ;
013800120302
013900120301 2     IF i00_idcliente <= *zeros ;
014000120301         Werror = ESITO_ERROR;
014100120306         rpyData = 'IDcliente non impostato'  ;
014200120830         //dump(a)  ;
014300120320         leavesr  ;
014400120302 2     ENDIF;
014500120302       on-error ;
014600120302
014700120302           Werror= ESITO_ERROR;
014800120306         rpyData = 'IDcliente non numerico'  ;
014900120830         //dump(a)  ;
015000120320         leavesr  ;
015100120302       endmon  ;
015200120305
015300120305       lingua= i00_liniso2        ;
015400120301 1     ENDIF;
015500120301
015600120302       // D E T T A G L I O   T A R I F F A
015700120302
015800120302 1     IF rqsOpCode = RQSOPCODE_GETTAMDETT ;
015900120302       reset tis7st01i;
016000120302
016100120302 2     SELECT;
016200120302         WHEN %SUBST(rqsData : 1 : 10) = i01_formato;
016300120302           tis7st01i = %SUBST(rqsData : 1 : rqsDataLen);
016400120302         OTHER;
016500120302           Werror= ESITO_ERROR;
016600120306         rpyData = 'Richiamo errato' ;
016700120830         //dump(a)  ;
016800120320         leavesr  ;
016900120302 2     ENDSL;
017000120302
017100120302       monitor  ;
017200120302
017300120302 2     IF i01_idcliente <= *zeros ;
017400120302         Werror = ESITO_ERROR;
017500120306         rpyData = 'IDcliente non impostato'  ;
017600120828         //dump(a)  ;
017700120320         leavesr  ;
017800120302 2     ENDIF;
017900120302
018000120306 2     IF i01_idcodtar  <  *zeros ;
018100120302         Werror = ESITO_ERROR;
018200120306         rpyData = 'Codice Tariffa non impostato'  ;
018300120320         leavesr  ;
018400120302 2     ENDIF;
018500120305
018600120302       on-error ;
018700120302           Werror= ESITO_ERROR;
018800120306         rpyData = 'IDcliente o cod tariffa non numerico'  ;
018900120830         //dump(a)  ;
019000120320         leavesr  ;
019100120302       endmon  ;
019200120302
019300120302 2     if i01_tipocli<>'M' and
019400120302          i01_tipocli<>'D' and
019500120306          i01_tipocli<>'O'    ;
019600120302           Werror= ESITO_ERROR;
019700120306         rpyData = 'Non indicato se cliente è Mittente Destinatario +
019800120306                    Ordinante'   ;
019900120830         //dump(a)  ;
020000120320         leavesr  ;
020100120302 2     endif  ;
020200120302
020300120305       lingua= i01_liniso2        ;
020400120302 1     ENDIF;
020500120305
020600120305       // Controllo e selezione lingua
020700120305       select ;
020800120305
020900120305       when  lingua ='en'   ;
021000120305        klin =2   ;
021100120305       when  lingua ='fr'   ;
021200120305        klin =3   ;
021300120305       when  lingua ='de'   ;
021400120305        klin =4   ;
021500120305       when  lingua ='it' or lingua='  '  ;
021600120305        klin =1   ;
021700120305       other ;
021800120305           Werror= ESITO_ERROR;
021900120306         rpyData = 'Codice lingua errato'  ;
022000120830         //dump(a)  ;
022100120305       endsl  ;
022200120301
022300120301      /END-FREE
022400120301     c                   ENDSR
022500120301     c*------------------------------------------------------------------*
022600120301     C*   Lettura tariffe per caricare shiere di output
022700120301     c*------------------------------------------------------------------*
022800120301     c     ElabTar       BEGSR
022900120627     c                   eval                    XX=100
023000120301      /FREE
0231001203011      if i00_dtspediz<>*blanks and i00_dtspediz<>*zeros ;
023200120301
023300120301       monitor  ;
023400120302         dataiso=%date(i00_DTSPEDIZ:  *eur)   ;
023500120301         on-error ;
023600120301           Werror= ESITO_ERROR;
023700120306         rpyData = 'data errata o non numerica';
023800120301       endmon  ;
023900120301
024000120301x1     else    ;
024100120301       dataiso=datasys   ;
024200120301 1     endif     ;
024300120301
024400120301 1     if Werror<>ESITO_ERROR;
024500120301        datarif=%dec(dataiso)   ;
024600120301
024700120627       clear  savctr  ;
024800120627       setgt  i00_idcliente tntam01l  ;
024900120627       readpe i00_idcliente tntam01l  ;
025000120301
025100120301 2     dow not %eof(tntam01l )  ;
025200120301
025300120305 3     if  tamatb= *blanks  ;
025400120301
025500120301       // elaboro solo le decorrenti alla data di riferimento
025600120627 4     if   tamddt<=datarif  ;
025700120627
025800120627       // se scaduta solo se ultimo progressivo
025900120627 4a    if   tamdst>=datarif  or
026000120627            %editc(tamctr:'X') <> savctr  ;
026100120627
026200120627       savctr = %editc(tamctr:'X' )  ;
026300120320
026400120320       //  elaboro solo se è da visulizzare in internet
026500120320 5     if tameds<>'N'  ;
026600120627       xx=xx-1   ;
026700120302
026800120627 6     if  xx>0    ;
026900120302       exsr Datitar   ;
027000120320 6     endif   ;
027100120302
027200120320 5     endif  ;
027300120627 4a    endif  ;
027400120627 4     endif  ;
027500120305 3     endif  ;
027600120301
027700120301
027800120627       readpe i00_idcliente tntam01l  ;
027900120301 2     enddo ;
028000120301
028100120301      /END-FREE
028200120301
028300120301       // Muovo le skiere nella Ds di output
028400120302     c                   reset                   tis7st00o
028500120627     c                   if        xx=100
028600120306     c                   eval      Werror = ESITO_ERROR
028700120306     c                   eval      rpyData = 'non esistono Tariffe per il clien+
028800120306     c                             te digitato'
028900120306     c                   else
029000120307     c                   eval      o00_formato='TIS7ST00O'
029100120627     c                   eval      o00_nrelementi= 100-xx
029200120627     c                   movea     codt(xx)      o00_skitariffa
029300120627     c                   movea     desT(xx)      o00_skidescriz
029400120627     c                   movea     desntw(xx)    o00_skinetwork
029500120627     c                   movea     destsp(xx)    o00_skitiposer
029600120301     c                   endif
029700120306     c                   endif
029800120301
029900120301     c                   ENDSR
030000120302     c*------------------------------------------------------------------*
030100120302     C*   Singola tariffa da caricare
030200120302     c*------------------------------------------------------------------*
030300120302     c     Singolatar    BEGSR
030400120302      /FREE
030500120305
030600120305       reset tis7st01o   ;
030700120627       clear xx  ;
030800120305
0309001203021      if i01_dtspediz<>*blanks and i01_dtspediz<>*zeros ;
031000120302
031100120302       monitor  ;
031200120302         dataiso=%date(i01_DTSPEDIZ:  *eur)   ;
031300120302         on-error ;
031400120302           Werror= ESITO_ERROR;
031500120306         rpyData = 'Data errata o non numerica';
031600120302       endmon  ;
031700120302
031800120302x1     else    ;
031900120302       dataiso=datasys   ;
032000120302 1     endif     ;
032100120302
032200120302 1     if Werror<>ESITO_ERROR;
032300120305       datarif=%dec(dataiso)   ;
032400120302
032500120627       setgt  (i01_idcliente:i01_idcodtar) tntam01l  ;
032600120627       readpe (i01_idcliente:i01_idcodtar) tntam01l  ;
032700120302
032800120302 2     dow not %eof(tntam01l )  ;
032900120302
033000120305 3     if  tamatb= *blanks  ;
033100120302
033200120305       // Elaboro la decorrente alla data di riferimento
033300120627 4     if  tamddt<=datarif ;
033400120320
033500120320 4a    if tameds<>'N'  ;
033600120302
033700120302       reset tis7st01o   ;
033800120627       xx=1  ;
033900120302       exsr DatiTar  ;
034000120305
034100120307       o01_formato='TIS7ST01O'  ;
034200120305       o01_IDDESCRIZ=dest(1) ;
034300120305       o01_IDNETWORK=ntw(1)  ;
034400120305       o01_IDDESCRNTW =desNTW(1)  ;
034500120305
034600120305       // Quantità da fattura visibile e obbligatoria per tariffe 4xx e 5xx
034700120305       o01_VISUALQTA='0'  ;
034800120305 5     if %subst(codt(1):1:1)='4' or %subst(codt(1):1:1)='5' ;
034900120305          o01_VISUALQTA='1'  ;
035000120305 5     endif   ;
035100120305
035200120305       // Nazione Mittente     necessaria per assegnato e tariffa estera
035300120305       o01_VISUALNZM='0'  ;
035400120305 5     if (i01_tipocli='O' or i01_tipocli='D') and ntw(1)<>*blanks  ;
035500120305          o01_VISUALnzm='1'  ;
035600120305 5     endif   ;
035700120305
035800120305       // Nazione Destinatario necessaria per franco    e tariffa estera
035900120305       o01_VISUALNZD='0'  ;
036000120305 5     if  i01_tipocli='M' and ntw(1)<>*blanks  ;
036100120305          o01_VISUALnzD='1'  ;
036200120305 5     endif   ;
036300120305
036400120305       leave  ;
036500120302
036600120320 4a    endif  ;
036700120320
036800120320       // se ho trovato la tariffa decorrente e non è utilizzabile esco dal ciclo
036900120320       leave  ;
037000120320 4     endif  ;
037100120305 3     endif  ;
037200120302
037300120627       readpe (i01_idcliente:i01_idcodtar) tntam01l  ;
037400120302 2     enddo ;
037500120306
037600120306       //  se non ho trovato la tariffa --> restituisco errore
037700120306       if xx=0  ;
037800120306           Werror= ESITO_ERROR;
037900120306         rpyData = 'Non trovato codice tariffa per il cliente'  ;
038000120306       endif  ;
038100120306
038200120306 1     endif  ;
038300120301
038400120302       ENDSR  ;
038500120305       //------------------------------------------------------------------*
038600120305       //   Dati da tariffa
038700120305       //------------------------------------------------------------------*
038800120302       BEGSR  DatiTar  ;
038900120302
039000120302       codt(XX)=%editc(tamctr:'X') ;
039100120302       dest(XX)=tamdcv  ;
039200120302       dsta01=tamflo   ;
039300120305 1     select  ;
039400120306         when  §tadpd='S'   ;
039500120305              NTW(xx) ='DPD'   ;
039600120302         when  §tafed='S'   ;
039700120305              NTW(xx) ='FED'   ;
039800120302         when  tamfie='E'   ;
039900120305              NTW(xx) ='EEX'   ;
040000120302         other   ;
040100120305              NTW(xx) ='   '   ;
040200120305 1     endsl  ;
040300120305
040400120305       // Decodifica network   ;
040500120305
040600120305 1     if   ntw(xx) <>*blanks  ;
040700120305          clear tbeke1  ;
040800120305          tbeke1=ntw(xx)  ;
040900120305
041000120305          chain  (' ': 'NTW':tbeke1 ) tntbe11l  ;
041100120305          desntw(xx)=tbeuni   ;
041200120305 x1    else  ;
041300120305
041400120305       // Traduco Italia nelle varie lingue dalla tabella Nazioni
041500120305       clear tblkey  ;
041600120305
041700120305       chain (klin :'15':tblkey)   tabel00f ;
041800120305 2     if %found(tabel00f)  ;
041900120305        ds5e=tbluni   ;
042000120305        desntw(XX)= §5edes  ;
042100120305 2     endif   ;
042200120302
042300120305 1     endif  ;
042400120305
042500120302       clear tblkey  ;
042600120302       tblkey=tamtsp  ;
042700120302
042800120305       chain (klin :'5E':tblkey)   tabel00f ;
042900120305 1     if %found(tabel00f)  ;
043000120302        ds5e=tbluni   ;
043100120302        destsp(XX)= §5edes  ;
043200120305 1     endif   ;
043300120302
043400120302       ENDSR   ;
043500120302
043600120302      /END-FREE
