000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300111018     Fficna5d   CF   E             WORKSTN
000500111018     Ffiadt01l  uF   E           k disk
000600111018     Ffifgt01l  uf   E           k disk
000700111019     Ffitgt02l  uF   E           k disk
000701111020     Ffitgt01l  uF   E           k disk    rename(fitgt000:fitgt1)
000800111018     Ffiatt01l  iF   E           k disk
000900111018     Ffiftt01l  iF   E           k disk
000901111019     Ffiapd01l  iF   E           k disk
001000011228      *
001001111018     d Tibs36Ds      e ds
001100020103     D Psds           SDS
001200020103     D  PgmName          *PROC
001300011228     D Kpjba         E DS
001400011228      *
002000011227     d data            ds            10
002100011228     d aa                      3      4
002200011228     d mm                      6      7
002300011228     d gg                      9     10
002400011228      *
002500030702     D fatto           S              1
002501111018     d newscad         s               d   datfmt(*iso)
003400081001     d dataiso         s               d   datfmt(*iso)
003500081001     d dataeur         s               d   datfmt(*eur)
003600081001     D WLBDA8          DS
003700081001     D  G02DAT                 1      8  0
003800081001     D  G02INV                 9     16  0
003900081001     D  G02ERR                17     17
004000081001     D  G02TGI                18     22  0
004001111018     c**********************************************************************
004002111018     c                   clear                   vpdr
004003111018     c                   clear                   vdatdec
004004111018     c                   clear                   vdatdecn
004005111019     c                   move      'A'           vtipo
004006111018     c                   do        *hival
004007111018     c                   exfmt     video1
004008111018     c                   setoff                                       28
004009111018     c   kc              exsr      uscita
004010111018     c                   clear                   vdpdr
004011111018      * codice autista
004012111018     c     kapd          chain     fiapd01l
004013111018     c                   if        %found(fiapd01l)
004014111018     c                   movel     apdrsf        vdpdr
004015111018     c                   else
004016111018     c                   eval      $msg = 'Codice Autista non trovato'
004017111018     c                   seton                                        28
004018111018     c                   iter
004019111018     c                   endif
004020111018      * inserimento date
004021111018     c                   if        vdatdec = 0 or vdatdecn = 0
004022111018     c                   eval      $msg = 'Inserire entrambe le date'
004023111018     c                   seton                                        28
004024111018     c                   iter
004025111018     c                   endif
004026111018      * controllo date
004027111019     c                   if        vdatdec> 0
004028111019     c                   move      vdatdec       g02dat
004029111019     c                   move      *blank        g02err
004030111019     C                   CALL      'XSRDA8'
004031111019     C                   PARM                    WLBDA8
004032111019     C     G02ERR        IFEQ      '1'
004033111019     C                   SETON                                        43
004034111019     C                   END
004035111019     c                   move      g02dat        vdatdec
004037111019     c                   move      g02inv        vdatdecg          8 0
004041111019     c                   endif
004042111019     c                   if        vdatdecn  > 0
004043111019     c                   move      vdatdecn      g02dat
004044111019     c                   move      *blank        g02err
004045111019     C                   CALL      'XSRDA8'
004046111019     C                   PARM                    WLBDA8
004047111019     C     G02ERR        IFEQ      '1'
004048111019     C                   SETON                                        43
004049111019     C                   END
004050111019     c                   move      g02dat        vdatdecn
004052111019     c                   move      g02inv        vdatdecng         8 0
004053111019     c                   endif
004057111018     c                   if        vdatdecng <= vdatdecg
004058111018     c                   eval      $msg = 'Data Nuova decorrenza errata deve -
004059111018     c                             essere maggiore di decorrenza attuale'
004060111018     c                   seton                                        28
004061111018     c                   iter
004062111018     c                   endif
004063111019      * imposta data per scadenza eventuale progressivo precedente
004064111019      * che diventa = a nuova decorrenza - 1 giorno
004065111107     c                   move      vdatdecng     dataiso
004066111107     c     dataiso       subdur    1:*d          newscad
004067111107      * effettuo verifiche per tipo tariffa
004068120208     c*                  if        *inkf
004069111018     c                   if        vtipo = 'A'
004070111107     c                   exsr      citta
004071111107     c                   else
004072111107     c                   exsr      affdef
004073111107     c                   endif
004074120208     c*                  endif
004075111107     c   28              iter
004900011227      *
004901111107     c                   enddo
005000011227     C                   EXSR      Uscita
005003111018      *_____________________________________________________________
005004111018     C     citta         BEGSR
005005111018      *_____________________________________________________________
005006111018      *
005007111019     c                   clear                   valorizzato       1
005008111018     C     vpdr          setll     fiftt01l
005009111018     C                   do        *hival
005010111018     c     vpdr          reade     fiftt01l
005011111018     c                   if        %eof(fiftt01l)
005012111018     c                   leave
005013111018     c                   endif
005014111019     c                   if        fttddc >= vdatdecg and fttddc < vdatdecng
005015111018     c                   move      'X'           valorizzato
005016111018     c                   leave
005017111018     c                   endif
005018111018     c                   enddo
005019111018     c                   if        valorizzato <> *blank
005020111018     c                   eval      $msg ='Esistono valorizzazioni per l''auti-
005021111018     c                             sta variazione non possibile'
005022111020     c                   seton                                        28
005023111018     c                   else
005024120208     c                   exsr      variatgt
005025111018     c                   endif
005026111018      *
005027111018     C                   ENDSR
005028111018      *_____________________________________________________________
005029111018     C     affdef        BEGSR
005030111018      *_____________________________________________________________
005031111018      *
005032111018     c                   clear                   valorizzato
005033111018     C     katt          setll     fiatt01l
005034111018     C                   do        *hival
005035111018     c                   read      fiatt01l
005036111019     c                   if        %eof(fiatt01l) or attddc > vdatdecng
005037111018     c                   leave
005038111018     c                   endif
005039111019     c                   if        attddc >= vdatdecg and attddc < vdatdecng
005040111018     c                             and attpdr = vpdr
005041111018     c                   move      'X'           valorizzato
005042111018     c                   leave
005043111018     c                   endif
005044111018     c                   enddo
005045111018     c                   if        valorizzato <> *blank
005046111018     c                   eval      $msg ='Esistono valorizzazioni per l''auti-
005047111018     c                             sta variazione non possibile'
005048111020     c                   seton                                        28
005049111018     c                   else
005050120208     c                   exsr      variaadt
005051111018     c                   endif
005052111018      *
005053111018     C                   ENDSR
005054111018      *_____________________________________________________________
005055111018     C     variatgt      BEGSR
005056111018      *_____________________________________________________________
005057111018      *aggiorna progressivo con nuova decorrenza
005058111018     c     vpdr          setgt     fitgt02l
005059111018     c     vpdr          readpe    fitgt02l
005060111019     c                   if        not%eof(fitgt02l)
005061111109     c                   if        vdatdecg <> tgtddt
005062111109     c                   eval      $msg ='Data decorrenza non corrispondente -
005063111109     c                             a quella della tariffa '
005064111109     c                   seton                                        28
005065111109     c                   leavesr
005066111109     c                   endif
005067111019     c                   exsr      aggfgtnew
005068111019     c                   z-add     vdatdecng     tgtddt
005069111019     c                   update    fitgt000
005070111018      *aggiorna progressivo precedente con nuova scadenza
005071111020     c                   eval      prgk =  tgtprg - 1
005072111020     c     kpdr          chain     fitgt01l
005073111020     c                   if        %found(fitgt01l)
005074111019     c                   exsr      aggfgtold
005075111019     c                   move      newscad       tgtdst
005076111020     c                   update    fitgt1
005077111018     c                   endif
005078111020     c                   else
005079111020     c                   eval      $msg = 'Tariffa non trovata o già stampata -
005080111020     c                              dalla sede modifica non possibile'
005081111020     c                   seton                                        28
005082111020     c                   endif
005083111018     C                   ENDSR
005084111018      *_____________________________________________________________
005085111019     C     aggfgtnew     begsr
005086111018      *_____________________________________________________________
005087111019     c     kfgt          setll     fifgt01l
005088111019     c                   do        *hival
005089111019     c     kfgt          reade     fifgt01l
005090111020     c                   if        %eof(fifgt01l)
005091111020     c                   leave
005092111020     c                   endif
005093111020     c                   if        tgtprg = fgtprg
005094111019     c                   z-add     vdatdecng     fgtddt
005095111019     c                   update    fifgt000
005096120208     c                   eval      $msg = 'Tariffa modificata con nuova -
005097120208     c                              decorrenza'
005098120208     c                   seton                                        28
005099111020     c                   endif
005100111019     c                   enddo
005101111019     C                   endsr
005102111019      *_____________________________________________________________
005103111019     C     aggfgtold     begsr
005104111019      *_____________________________________________________________
005105111019     c     kfgt          setll     fifgt01l
005106111019     c                   do        *hival
005107111019     c     kfgt          reade     fifgt01l
005108111020     c                   if        %eof(fifgt01l)
005109111020     c                   leave
005110111020     c                   endif
005111111020     c                   if        tgtprg = fgtprg
005112111019     c                   move      newscad       fgtdst
005113111019     c                   update    fifgt000
005114111020     c                   endif
005115111019     c                   enddo
005116111019     C                   endsr
005117111019      *_____________________________________________________________
005118111019     C     variaadt      begsr
005119111019      *_____________________________________________________________
005120111019      *aggiorna progressivo con nuova decorrenza
005121111019     c     vpdr          setgt     fiadt01l
005122111019     c     vpdr          readpe    fiadt01l
005123111020     c                   if        not%eof(fiadt01l) and adtdcn = 0
005124111109     c                   if        vdatdecg <> adtddt
005125111109     c                   eval      $msg ='Data decorrenza non corrispondente -
005126111109     c                             a quella della tariffa '
005127111109     c                   seton                                        28
005128111109     c                   leavesr
005129111109     c                   endif
005130111019     c                   z-add     vdatdecng     adtddt
005131111019     c                   update    fiadt000
005132111019      *aggiorna progressivo precedente con nuova scadenza
005133111019     c     vpdr          readpe    fiadt01l
005134111019     c                   if        not%eof(fiadt01l)
005135111019     c                   move      newscad       adtdst
005136111019     c                   update    fiadt000
005137120208     c                   eval      $msg = 'Tariffa modificata con nuova -
005138120208     c                              decorrenza'
005139120208     c                   seton                                        28
005140111019     c                   endif
005141111020     c                   else
005142111020     c                   eval      $msg = 'Tariffa non trovata o già stampata -
005143111020     c                              dalla sede modifica non possibile'
005144111020     c                   seton                                        28
005145111020     c                   endif
005146111019     C                   endsr
005147111019      *_____________________________________________________________
005200011227     c* uscita
005201111018      *_____________________________________________________________
005400011227     C     Uscita        BEGSR
005500011227      *
005600011227     C                   EVAL      *INLR = *ON
005700011227     C                   RETURN
005800011227      *
005900011227     C                   ENDSR
042700011227     c**********************************************************************
042800000607     C     *INZSR        BEGSR
042900011227     c**********************************************************************
043000000607      *
043100000607     C     *ENTRY        PLIST
043200000607     C                   PARM                    Kpjba
043400080930
044000030703     C                   MOVEL     knsif         vknsif
044100030703     C                   MOVEL     knmus         vknmus
044200030702      *reperimento data
044300030702     C                   TIME                    W0120            14 0
044400030702     C                   MOVE      W0120         WDAT              8 0
044500030702     C*
044600030702     C                   Z-ADD     WDAT          G02DAT
044700030702     C                   MOVEL     *BLANK        G02ERR
044800030702     C                   CALL      'XSRDA8'
044900030702     C                   PARM                    WLBDA8
045000030702     C* UDATE A 8 IN AAAA/MM/GG
045100030702     C                   Z-ADD     G02INV        WUDA              8 0
045200080930     C                   CLEAR                   Tibs36Ds
045300080930     C                   EVAL      I36ute = knmus
045400080930     C                   CALL      'TIBS36R'
045500080930     C                   PARM                    tibs36ds
045600030718
045700030718     C     Kapd          KLIST
045800111018     C                   KFLD                    vtipo
045900111018     C                   KFLD                    vpdr
045901111018     C     Katt          KLIST
045902111018     C                   KFLD                    kter              3 0
045903111018     C                   KFLD                    kddc              8 0
045904111019     C     Kfgt          KLIST
045905111019     C                   KFLD                    tgtPDR
045906111019     C                   KFLD                    tgtSML
045907111020     C     Kpdr          KLIST
045908111020     C                   KFLD                    tgtPDR
045909111020     C                   KFLD                    smlk              1
045910111020     C                   KFLD                    prgk              3 0
046100000607      *
050000000607      *---------------------------------------------------------------------
050100011227     C                   ENDSR
