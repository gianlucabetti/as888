000100050301      *---------------------------------------------------------------*
000200050301      *? Cancellazione vecchi spool da outq (da AZCSP00F)            ?*
000300050301      *---------------------------------------------------------------*
000400050301      * Il parametro in input è facoltativo:                          *
000500050301      * se ricevuto servono a selezionare solo  i rec. CSPTYP=PTYP    *
000600050301      * se NON ricevuto vengono elaborati tutti i rec. CSPTYP=*blanks *
000700050301      *---------------------------------------------------------------*
000800050301     h decedit('0,') datedit(*dmy/)
000900050301      *
001000050405      *?A R C H I V I?------------------------------------------------*
001100050405      *
001200050301     fAZCSP01L  Uf   e           k disk
001300050405      *
001400050405      *?C O S T A N T I?----------------------------------------------*
001500050405      *
001600050405      *
001700050405      *?S C H I E R E?------------------------------------------------*
001800050405      *
001900050301      *
002000050405      *?S T R U T T U R E   D A T I?----------------------------------*
002100050301      *
002200050301     d WLBdat          ds                  inz
002300050301     d  G02dat                 1      8  0 inz
002400050301     d  G02inv                 9     16  0 inz
002500050301     d  G02err                17     17    inz('3')
002600050301     d  G02tgi                18     22  0 inz
002700050301      *
002800050301     d Status         sds           333
002900050301     d  SDSpgm           *proc
003000050301     d  SDSprm           *parms
003100050301      *
003200050405      *?V A R I A B I L I?--------------------------------------------*
003300050301      *
003400050719     d $ASbackup       s              1    inz(*off)
003500050301     d WdateISO        s               d   inz   datfmt(*iso)
003600050301     d Wdate8          s              8  0 inz
003700050301     d Wyear           s              4  0 inz
003800050405      *
003900050406      * Parametri x pgm. TRUL45R
004000050405     d PRPuser         s             10
004100050405     d PRPoutq         s             10
004200050405     d PRPoutqlib      s             10
004300050405     d PRPformtype     s             10
004400050405     d PRPusrdta       s             10
004500051124     d PRPstatus       s              1
004600050405     d PRPdatpul       s              7
004700051123     d PRPrifdtp       s              1
004800050405     d PRPlista        s              1
004900050405      *
005000050405      *?I N D I C A T O R I?------------------------------------------*
005100050405      *
005200050405      *  21  -  Comodo
005300050406      *  90  -  Errore nel richiamo del pgm. TRUL45R
005400050405      *
005500050405      *---------------------------------------------------------------*
005600050301      *
005700050405      *?C I C L O   D I   E L A B O R A Z I O N E?--------------------*
005800050301      *
005900050301     c     *Entry        plist
006000050708     c                   parm                    PRMlista          1
006100050405     c                   parm                    PRMtyp            3
006200050301      *
006300050405      *?Operazioni iniziali?
006400050719      *
006500050719      * Verifica se AS/400 di backup
006600050719     c                   call      'TNSY14C'
006700050719     c                   parm                    $ASbackup
006800050708      *
006900050708      * Impostazione richiesta di lista e/o effettiva cancellazione
007000050708      *   (v. parm PRMlista: "1" = solo lista spool eliminabili
007100050708      *                      "2" = solo eliminazione spool
007200050708      *                      "3" = lista ed eliminazione spool)
007300050708     c                   if             SDSprm   > *zeros
007400050708     c                             and (PRMlista = '1'
007500050708     c                              or  PRMlista = '2'
007600050708     c                              or  PRMlista = '3')
007700050708     c                   eval      PRPlista = PRMlista
007800050708     c                   else
007900050708     c                   eval      PRPlista = '3'
008000050708     c                   endif
008100050301      *
008200050405      * Impostazione tipo record da elaborare
008300050708     c                   if        SDSprm <= 1
008400050405      * - Elaborazione di tutti i  rec. di AZCSP00F con CSPtyp = *blanks
008500050405      *   (spool)
008600050301     c                   clear                   CSPtyp
008700050301     c                   else
008800050405      * - Elaborazione del singolo rec. di AZCSP00F con CSPtyp = PTYP
008900050405      *   (parametro ricevuto)
009000050405     c                   movel     PRMtyp        CSPtyp
009100050301     c                   endif
009200050301      *
009300050405      *?Ciclo di lettura del file AZCSP00F?
009400050301     c     CSPtyp        setll     AZCSP000
009500050301     c     CSPtyp        reade     AZCSP000
009600050301      *
009700050301do  1c                   doW       NOT %eof(AZCSP01L)
009800050301      *
009900050301      * - elaborazione del singolo record in esame
010000050301if  2c                   if        CSPatb = *blanks
010100050719     c                             and ($ASbackup  =  *off
010200050719     c                              or  $ASbackup  =  *on
010300050719     c                              and CSPdltbkup <> 'N'
010400050719     c                              or  $ASbackup  =  *on
010500050719     c                              and CSPdltbkup =  'N'
010600050719     c                              and PRPlista   =  '1')
010700050301     c                   exsr      Elab1outq
010800050301e   2c                   endif
010900050301      *
011000050301     c     CSPtyp        reade     AZCSP000
011100050301      *
011200050301e   1c                   enddo
011300050301      *
011400050405      *?Fine?
011500050301     c                   eval      *inLR  = *on
011600050301      *
011700050405      *?S U B R O U T I N E S?----------------------------------------*
011800050301      *
011900050405      *---------------------------------------------------------------*
012000050405      *?Elaborazione singola outq                                    ?*
012100050405      *---------------------------------------------------------------*
012200050301     c     Elab1outq     BegSr
012300050301      *
012400050719      * Reperimento data odierna
012500050719     c                   move      *date         WdateISO
012600050301      * calcolo data (secolo compreso) fino a cui cancellare...
012700050301     c                   subdur    CSPggpul:*d   WdateISO
012800050301     c                   move      WdateISO      Wdate8
012900050405     c                   move      Wdate8        PRPdatpul
013000050301     c                   extrct    WdateISO:*Y   Wyear
013100050301     c                   if        Wyear >= 2000
013200050405     c                   movel     '1'           PRPdatpul
013300050301     c                   endif
013400050708      * ho già impostato il richiamo con lista degli spool eliminati;
013500050708      *   (v. parm PRPlista: "1" = solo lista spool eliminabili
013600050708      *                      "2" = solo eliminazione spool
013700050708      *                      "3" = lista ed eliminazione spool)
013800050301      *
013900050301      * richiamo pgm x cancellazione vecchi spool da outq generiche
014000050406     c                   call      'TRUL45R'                            90
014100050405     c                   parm      CSPuser       PRPuser
014200050405     c                   parm      CSPoutq       PRPoutq
014300050405     c                   parm      CSPoutqlib    PRPoutqlib
014400050405     c                   parm      CSPfrmtyp     PRPformtype
014500050405     c                   parm      CSPusrdta     PRPusrdta
014600051124     c                   parm      CSPstatus     PRPstatus
014700050405     c                   parm                    PRPdatpul
014800051123     c                   parm      CSPrifggp     PRPrifdtp
014900050708     c                   parm                    PRPlista
015000050301      *
015100050301      * aggiornamento archivio AZCSP00F (se non si sono stati errori)
015200050301if  1c                   if        NOT *in90
015300050406     c                             and PRPlista <> '1'
015400050301     c                   move      WdateISO      CSPdup
015500050301     c                   UPDATE    AZCSP000
015600050301x   1c                   else
015700050301     c                   UNLOCK    AZCSP01L
015800050301e   1c                   endif
015900050301      *
016000050301     c                   EndSr
