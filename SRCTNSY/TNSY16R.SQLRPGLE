000100161129     /*END
000200050301      *---------------------------------------------------------------*
000300050301      *? Cancellazione vecchi spool da outq (da AZCSP00F)            ?*
000400050301      *---------------------------------------------------------------*
000500050301      * Il parametro in input è facoltativo:                          *
000600161129      * se ricevuto servono a selezionare solo  i rec. CSPTYP=PRMTYP  *
000700050301      * se NON ricevuto vengono elaborati tutti i rec. CSPTYP=*blanks *
000800050301      *---------------------------------------------------------------*
000900050301     h decedit('0,') datedit(*dmy/)
001000050301      *
001100050405      *?A R C H I V I?------------------------------------------------*
001200050405      *
001300050301     fAZCSP01L  Uf   e           k disk
001400050405      *
001500050405      *?C O S T A N T I?----------------------------------------------*
001600050405      *
001700050405      *
001800050405      *?S C H I E R E?------------------------------------------------*
001900050405      *
002000050301      *
002100050405      *?S T R U T T U R E   D A T I?----------------------------------*
002200050301      *
002300050301     d WLBdat          ds                  inz
002400050301     d  G02dat                 1      8  0 inz
002500050301     d  G02inv                 9     16  0 inz
002600050301     d  G02err                17     17    inz('3')
002700050301     d  G02tgi                18     22  0 inz
002800050301      *
002900050301     d Status         sds           333
003000050301     d  SDSpgm           *proc
003100050301     d  SDSprm           *parms
003200050301      *
003300050405      *?V A R I A B I L I?--------------------------------------------*
003400050301      *
003500050719     d $ASbackup       s              1    inz(*off)
003600050301     d WdateISO        s               d   inz   datfmt(*iso)
003700050301     d Wdate8          s              8  0 inz
003800050301     d Wyear           s              4  0 inz
003900161129     d wDayWeek        s              1  0 inz
004000050405      *
004100050406      * Parametri x pgm. TRUL45R
004200050405     d PRPuser         s             10
004300050405     d PRPoutq         s             10
004400050405     d PRPoutqlib      s             10
004500050405     d PRPformtype     s             10
004600050405     d PRPusrdta       s             10
004700051124     d PRPstatus       s              1
004800050405     d PRPdatpul       s              7
004900051123     d PRPrifdtp       s              1
005000050405     d PRPlista        s              1
005100050405      *
005200050405      *?I N D I C A T O R I?------------------------------------------*
005300050405      *
005400050405      *  21  -  Comodo
005500050406      *  90  -  Errore nel richiamo del pgm. TRUL45R
005600050405      *
005700050405      *---------------------------------------------------------------*
005800050301      *
005900050405      *?C I C L O   D I   E L A B O R A Z I O N E?--------------------*
006000050301      *
006100050301     c     *Entry        plist
006200050708     c                   parm                    PRMlista          1
006300161129     c                   parm                    PRMcancInSett     1
006400050405     c                   parm                    PRMtyp            3
006500050301      *
006600050405      *?Operazioni iniziali?
006700050719      *
006800050719      * Verifica se AS/400 di backup
006900050719     c                   call      'TNSY14C'
007000050719     c                   parm                    $ASbackup
007100161129      *
007200161129      /free
007300161129         // -?Reperimento giorno della settimana?
007400161129         //  ?(SE richiesta cancellazione "differenziata" infrasettimanale)?
007500161129         clear  wDayWeek;
007600161129         if  %parms()      >= 2  and
007700161129             PRMcancInSett <> *blank;
007800161129           // ·?1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6=Sat, 7=Sun?
007900161129           exec sql  set :wDayWeek = dayOfWeek_iso(curdate());
008000161129         else;
008100161129           wDayWeek = 9;
008200161129         endif;
008300161129      /end-free
008400050708      *
008500050708      * Impostazione richiesta di lista e/o effettiva cancellazione
008600050708      *   (v. parm PRMlista: "1" = solo lista spool eliminabili
008700050708      *                      "2" = solo eliminazione spool
008800050708      *                      "3" = lista ed eliminazione spool)
008900050708     c                   if             SDSprm   > *zeros
009000050708     c                             and (PRMlista = '1'
009100050708     c                              or  PRMlista = '2'
009200050708     c                              or  PRMlista = '3')
009300050708     c                   eval      PRPlista = PRMlista
009400050708     c                   else
009500050708     c                   eval      PRPlista = '3'
009600050708     c                   endif
009700050301      *
009800050405      * Impostazione tipo record da elaborare
009900161129     c                   if        SDSprm <= 2
010000050405      * - Elaborazione di tutti i  rec. di AZCSP00F con CSPtyp = *blanks
010100050405      *   (spool)
010200050301     c                   clear                   CSPtyp
010300050301     c                   else
010400050405      * - Elaborazione del singolo rec. di AZCSP00F con CSPtyp = PTYP
010500050405      *   (parametro ricevuto)
010600050405     c                   movel     PRMtyp        CSPtyp
010700050301     c                   endif
010800050301      *
010900050405      *?Ciclo di lettura del file AZCSP00F?
011000050301     c     CSPtyp        setll     AZCSP000
011100050301     c     CSPtyp        reade     AZCSP000
011200050301      *
011300050301do  1c                   doW       NOT %eof(AZCSP01L)
011400050301      *
011500050301      * - elaborazione del singolo record in esame
011600050301if  2c                   if        CSPatb = *blanks
011700161129     c                             and (CSPggPul   <= 5
011800161129     c                              or  wDayWeek   >= 6)
011900050719     c                             and ($ASbackup  =  *off
012000050719     c                              or  $ASbackup  =  *on
012100050719     c                              and CSPdltbkup <> 'N'
012200050719     c                              or  $ASbackup  =  *on
012300050719     c                              and CSPdltbkup =  'N'
012400050719     c                              and PRPlista   =  '1')
012500050301     c                   exsr      Elab1outq
012600050301e   2c                   endif
012700050301      *
012800050301     c     CSPtyp        reade     AZCSP000
012900050301      *
013000050301e   1c                   enddo
013100050301      *
013200050405      *?Fine?
013300050301     c                   eval      *inLR  = *on
013400050301      *
013500050405      *?S U B R O U T I N E S?----------------------------------------*
013600050301      *
013700050405      *---------------------------------------------------------------*
013800050405      *?Elaborazione singola outq                                    ?*
013900050405      *---------------------------------------------------------------*
014000050301     c     Elab1outq     BegSr
014100050301      *
014200050719      * Reperimento data odierna
014300050719     c                   move      *date         WdateISO
014400050301      * calcolo data (secolo compreso) fino a cui cancellare...
014500050301     c                   subdur    CSPggpul:*d   WdateISO
014600050301     c                   move      WdateISO      Wdate8
014700050405     c                   move      Wdate8        PRPdatpul
014800050301     c                   extrct    WdateISO:*Y   Wyear
014900050301     c                   if        Wyear >= 2000
015000050405     c                   movel     '1'           PRPdatpul
015100050301     c                   endif
015200050708      * ho già impostato il richiamo con lista degli spool eliminati;
015300050708      *   (v. parm PRPlista: "1" = solo lista spool eliminabili
015400050708      *                      "2" = solo eliminazione spool
015500050708      *                      "3" = lista ed eliminazione spool)
015600050301      *
015700050301      * richiamo pgm x cancellazione vecchi spool da outq generiche
015800050406     c                   call      'TRUL45R'                            90
015900050405     c                   parm      CSPuser       PRPuser
016000050405     c                   parm      CSPoutq       PRPoutq
016100050405     c                   parm      CSPoutqlib    PRPoutqlib
016200050405     c                   parm      CSPfrmtyp     PRPformtype
016300050405     c                   parm      CSPusrdta     PRPusrdta
016400051124     c                   parm      CSPstatus     PRPstatus
016500050405     c                   parm                    PRPdatpul
016600051123     c                   parm      CSPrifggp     PRPrifdtp
016700050708     c                   parm                    PRPlista
016800050301      *
016900050301      * aggiornamento archivio AZCSP00F (se non si sono stati errori)
017000050301if  1c                   if        NOT *in90
017100050406     c                             and PRPlista <> '1'
017200050301     c                   move      WdateISO      CSPdup
017300050301     c                   UPDATE    AZCSP000
017400050301x   1c                   else
017500050301     c                   UNLOCK    AZCSP01L
017600050301e   1c                   endif
017700050301      *
017800050301     c                   EndSr
