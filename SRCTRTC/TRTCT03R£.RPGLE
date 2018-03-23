000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200090223     H* TRTCT03R*----------------------------------------------------*
000300900418     H*                                                              *
000400090223     H* Ricevuto IFTMIN da Nilfisk  si deve dare l'info di ricevuto  *
000500090223     H*   con EDI si deve scrivere questo file per dire che il file  *
000600090223     H*   è arrivato .  (senza entrare nel merito degli errori)      *
000700090223     H*     .....al momento ......                                   *
000800060811      **?___________________________________________________________ *
000900080424     ftivgd00f  if a E             DISK    commit
001000080529      **?___________________________________________________________ *
001100060810
001200090223     D CTRLRecord      S           2048
001300060810     d trul47ds      e ds
001400060810     D W0140           S             14  0 inz
001500060810      *
001600900418     D KPJBA         E DS
001700060811     D Almeno_uno      S              1
001800050704
001900050705     D KPJBus          s                   like(kpjbu)
002000060811     D solo_inizio     s              1    inz(' ')
002100090223      **====================================================================*/
002200090303     D Segmento_UNA    C                   'UNA:+.?'
002300090303     D Segmento_UNB    C                   'UNB+UNOC:3+ITBAR005:ZZ'
002400090223     D Segmento_UNH    C                   'UNH+1+CONTRL:D:96A:UN:EAN002'
002500090223     D Segmento_UCI    C                   '+ITBAR005:ZZ+1'
002600090223     D Segmento_UNT    C                   'UNT+3+1'
002700090223     D Segmento_UNZ    C                   'UNZ+1+1'
002800090223      **?================================================================== */
002900090223      *    SCRITTURA DEL MESSAGGIO senza dettaglio errori
003000090223      **?================================================================== */
003100090223      * Schema messaggio:    UNA.....
003200090223      *                      UNB......
003300090223      *                      UNH......
003400090223      *                      UCI.....
003500090223      *                      UCM.....
003600090223      *                       "
003700090223      *                      UNT.....
003800090223      *                      UNZ.....
003900090223      **?================================================================== */
004000090223     c     *entry        plist
004100090226     C                   parm                    MITTENTE_UNB     35
004200090226     C                   parm                    MITTENTE_QUAL     4
004300090223     C                   parm                    ID_MESSAGGIO     14
004400090223      ***
004500090223
004600090223     C                   TIME                    ORADAT           14 0
004700090223     C                   MOVEL     ORADAT        ORATR             6 0
004800090223     C                   MOVEL     ORATR         ORAtra            4
004900090223     c                   move      *date         udtymd            8 0
005000090223     c                   move      udtymd        DATtra            6
005100090223     c                   move      udtymd        udtymda           8
005200090223     C                   TIME                    W0140
005300090223     C                   MOVE      W0140         W0080             8 0
005400090223      *----------------------------------------------------*
005500060811      **
005600090223      *  UNA......
005700090223     c                   clear                   CTRLRecord
005800090303     c                   eval      CTRLRecord = Segmento_UNA + ' '''
005900090223     C                   exsr      tivgd
006000090223      *
006100090223      *  UNB......
006200090223     c                   clear                   CTRLRecord
006300090223     c                   eval      CTRLRecord = Segmento_UNB    + '+' +
006400090226     c                                          %Trim(MITTENTE_UNB) + ':' +
006500090226     c                                          %Trim(MITTENTE_QUAL)+ '+' +
006600090223     c                                          datTRA + ':' +
006700090223     c                                          oraTRA + '+1'''
006800090223     C                   exsr      tivgd
006900090223      *
007000090223      *  UNH......
007100090223     c                   clear                   CTRLRecord
007200090223     c                   eval      CTRLRecord = Segmento_UNH + ''''
007300090223     C                   exsr      tivgd
007400090223      *
007500090223      *  UCI......
007600090223      *
007700090223     c                   clear                   CTRLRecord
007800090223     c                   eval      CTRLRecord = 'UCI+' +
007900090223     c                                          %Trim(ID_MESSAGGIO) + '+' +
008000090226     c                                          %Trim(MITTENTE_UNB) + ':' +
008100090226     c                                          %Trim(MITTENTE_QUAL)+
008200090223     c                                          Segmento_UCI + ''''
008300090223     C                   exsr      tivgd
008400090223      *
008500090223      *
008600090223      *  UNT......
008700090223     c                   clear                   CTRLRecord
008800090223     c                   eval      CTRLRecord = Segmento_UNT + ''''
008900090223     C                   exsr      tivgd
009000090223      *
009100090223      *
009200090223      *  UNZ......
009300090223     c                   clear                   CTRLRecord
009400090223     c                   eval      CTRLRecord = Segmento_UNZ + ''''
009500090223     C                   exsr      tivgd
009600090223      *
009700060810     C*
009800060810     C* Infine elimino il blocco elaborazione TIVGD
009900060811     c                   if        almeno_uno = 'S' and solo_inizio = 'S'
010000060810     C                   clear                   trul47ds
010100060810     C                   eval      d47opz  = 'F'
010200090224     C                   eval      d47tip  = 'EL'
010300060810     C                   call      'TRUL47R'
010400060810     C                   parm                    trul47ds
010500060811     c                   endif
010600050704      *
010700050705     c                   seton                                        lr
010800090223      **?__________________________________________________________________ */
010900090223      *   Scrittura tivgd file da trsmettere               *
011000090223      **?__________________________________________________________________ */
011100090223     C     tivgd         Begsr
011200090223      *
011300090223     c                   clear                   tivgd000
011400090223     c                   eval      vgddta = %TrimR(CTRLRecord)
011500090224     c                   eval      vgdtip = 'EL'
011600090224     c                   eval      vgdksu = '02494114'
011700090223     c                   eval      vgdtsc = 'WW'
011800090223     c                   eval      vgdpgm = 'TRTCT03R'
011900090223     c                   eval      vgddat = udtymd
012000090223      *
012100090223     C                   WRITE     tivgd000
012200090223      *
012300090223     C                   Endsr
012400050920      **?__________________________________________________________________ */
