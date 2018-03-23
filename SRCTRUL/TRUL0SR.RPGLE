000100020412     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020412
000300050518      * TRUL0SR  *---------------------------------------------------*
000400050518      * - controlla se schiera piena e msg di avvertimento
000500020412      *--------------------------------------------------------------*
000600050518     D MSG             S             70    DIM(2) CTDATA PERRCD(1)
000700020412
000800050518      * - Parametri x Controllo profilo utenti
000900050518     d TIBS34ds      e ds
001000050518      * - Ds di riferimento al file esterno AZUTE00F
001100050518     d AZUTEds       e ds                  extname(AZUTE00F)
001200050518      * - Ds per dati organigramma
001300050518     d dDATIUTE      e ds
001400050518     D KPJBA         E DS
001500050518     d trul0sds      e ds
001600050518     d dVpo          e ds
001700050518     D* DS PER TIBS02R - GESTIONE TNTBE00F
001800050518     D DSBS02        E DS                  EXTNAME(TIBS02DS)
001900050518     d
002000050518     d* Definizione campi
002100050518     d wCARICA         s              1    INZ(' ')
002200050518     d wperce          s              3  0
002300050518     d alfperce        s              3
002400050523     d alfele          s             11
002500050523     d alfpie          s             11
002600050518     d
002700020412      *---------------------------------------------------------------
002800020412     c     *entry        plist
002900050518     c                   parm                    KPJBA
003000050518     c                   movel     kpjbu         trul0Sds
003100020412
003200050518      * i0stla = ' '  Elaboro e chiudo con RETURN
003300050518      * i0stla = 'L'  Elaboro e chiudo con LR
003400050518      * i0stla = 'C'            chiudo con LR  solo per chiusura file
003500020412
003600050518    1c                   if        i0stla = 'C'
003700050518     c                   eval      *inlr =*on
003800050518     c                   else
003900050518     c* CARICO DATI UNA VOLTA
004000050519     C                   IF        WCARICA=' '
004100050518     C                   EXSR      CARICA
004200050518     C                   ENDIF
004300050518     c
004400050518     c* controllo se ci sono tutti i dati
004500050518     c                   if        i0sele=0
004600050518     c                   eval      o0smsg=msg(1)
004700050518     c                   goto      fine
004800050518     c                   endif
004900050518     C
005000050518     C* CALCOLO % riempimento della skiera
005100050518     c                   if        i0spie>0
005200050524     c
005300050524     c                   if        i0spie>i0sele
005400050524     c                   eval      wperce=150
005500050524     c                   else
005600050518     c                   eval(H)   wperce=(100*i0spie)/i0sele
005700050524     c                   endif
005800050524     c
005900050518     c                   else
006000050518     c                   clear                   wperce
006100050524     c                   endif
006200050518     c
006300050518     c                   if        wperce>=§vporsk
006400050518     c* invio messaggio
006500050518     C                   EXSR      invia
006600050518     c                   endif
006700050518     c
006800020412
006900050518     c     fine          tag
007000050518     c
007100050518    2c                   if        i0stla = 'L'
007200020412     c                   eval      *inlr = *on
007300020412   X2c                   else
007400020412     c                   return
007500020412    2c                   endif
007600020412
007700020412     c                   endif
007800020412
007900020412      *---------------------------------------------------------------*
008000050518      * INVIO MESSAGGIO DI RIEMPIMENTO IN POSTA AS
008100020412      *---------------------------------------------------------------*
008200050518     c     INVIA         begsr
008300050518     c
008400050518     C* UTENTE A CUI INVIARE: se non passato lo determino
008500050518     c                   if        i0sute=*blanks
008600050518     c                   eval      i0sute=uteute
008700050518     c                   endif
008800050518     c*
008900050518     c                   eval      alfperce=(%editc(wperce: '4'))
009000050523     c                   eval      alfele=(%editc(i0sele: '2'))
009100050523     c                   eval      alfpie=(%editc(i0spie: '2'))
009200101111
009300101111     C                   movel     *blanks       wrkEml          253
009400101111     C                   movel     *blanks       wrkMsg         5000
009500101111     C                   movel     *blanks       wrkOgg           44
009600101111     C* Valorizzo i campi della e-m@ail
009700120302     C                   eval      wrkEml= 'elisa.sanfelici@brt.it'
009800050518     c
009900101111     C* oggetto
010000101111     c                   eval      wrkOgg='SCHIERA IN RIEMPIMENTO NE'+
010100050523     C                             'L PGM : '+ I0SPGM
010200050518     c
010300101111     c* Messaggio
010400050518     c* SE non presente cod tabella, si tratta di file per cui msg diverso
010500050518     c                   if        i0scod<>*blanks
010600101111     c                   eval      wrkmsg= 'N o m e  SCHIERA :'+ i0sski + '    '
010700101111     c                             +'Caricata dalla  TABELLA :'+
010800050524     c                             ' " ' + i0scod +'"  del File  ' + i0sfile
010900101111     c                             +'    '+'Sistema  Informativo : '+ i0ssif
011000101111     c                             +'    '+'Schiera  piena per il '+
011100050523     c                             alfperce+'%'
011200101111     c                             +'                                       '
011300101111     c                             +'Elementi pieni/totale :'+ alfpie +
011400050523     c                             ' /' +alfele
011500101111     c                             +'    '+'Utente :'+ i0sute
011600050518     c                   else
011700050518     c
011800101111     c                   eval      wrkmsg='N o m e  SCHIERA .....: '+ i0sski
011900101111     c                             +'    '+'Caricata dal  F I L E : '+
012000050523     c                             i0sfile
012100101111     c                             +'    '+'Sistema  Informativo : '+ i0ssif
012200101111     c                             +'    '+'Schiera  piena per il'+
012300050523     c                             alfperce+'%'
012400101111     c                             +'    '+'Elementi pieni/totale :'+ alfpie+
012500050523     c                             ' /' +alfele
012600101111     c                             +'    '+'Utente : '+ i0sute
012700101111     c                   endif
012800101111
012900101111     C                   call(e)   'TIS701C'
013000101111     C                   parm                    wrkEml
013100101111     C                   parm                    wrkOgg
013200101111     C                   parm                    wrkMsg
013300101111
013400020412     c                   endsr
013500050518     c*--------------------------------------------------------------------
013600050518     c     CARICA        begsr
013700050518     c
013800050518      * Reperisco nome azienda per stampa PREPAGATI
013900050518     c     *dtaara       define    §azute        AZUTEds
014000050518     c     *dtaara       define    §datiute      dDATIUTE
014100050518     c                   in(E)     *dtaara
014200050518     c                   if        %ERROR or RSUT = *blanks
014300050518     c                   clear                   Tibs34Ds
014400050518     c                   call      'TIBS34R'
014500050518     c                   parm                    Tibs34Ds
014600050518     c                   in        *dtaara
014700050518     c                   endif
014800050518     c
014900050518      * Aggancio tabella "VPO" per % riemepimento
015000050518     C                   CLEAR                   dsbs02
015100050518     c                   clear                   dvpo
015200050518     C                   MOVEL     'C'           T02MOD
015300050518     c                   if        i0ssif<>*blanks
015400050518     C                   MOVEL     i0ssif        T02SIF
015500050518     c                   else
015600050518     C                   MOVEL     KNSIF         T02SIF
015700050518     c                   endif
015800050518     C                   MOVEL     'VPO'         T02COD
015900050518     c                   movel(P)  'VPO'         t02ke1
016000050518     C                   CALL      'TIBS02R'
016100050518     C                   PARM                    KPJBA
016200050518     C                   PARM                    dsbs02
016300050518    2C                   IF        T02ERR = *BLANKS
016400050518     C                   MOVEL     T02uni        dvpo
016500050518    2C                   ENDIF
016600050518     C
016700050518     C                   EVAL      WCARICA='1'
016800050518     c                   endsr
016900050518**         MSG
017000050518Numero elementi skiera non passato: impossibile il calcolo
