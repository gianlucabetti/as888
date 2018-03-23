000100011011      *
000200011011      *  Routine controllo decimali
000300011011      * ----------------------------
000400011011      *
000500011011      *  Errori previsti (OcdEsi <> *off) :
000600011011      *  >>>>>> Errori bloccanti:
000700011011      *  '1' = Rilevati decimali oltre il limite segnalato
000800011011      *  '9' = Parametri errati o mancanti
000900011011      *  >>>>>> Errori non bloccanti:
001000011011      *
001100011011
001200011011     H DECEDIT('0,') DATEDIT(*DMY.)
001300011011
001400011011      * Schiera messaggi
001500011011     D MSG             S             80    dim(5) ctdata perrcd(1)              MSG VIDEO
001600011011
001700011011      * Parametri I/O
001800011012     D XDECds        e ds
001900011011
002000011011     D* Campi di lavoro
002100011012     D wNumN           s             30 30 inz
002200011011     D wNumA           s             30    inz(*zeros)
002300011011     D wNrDec          s              2    inz
002400011011     D wNrDec1         s              1    inz(*zeros)
002500011011     D wNrDec2         s              2    inz(*zeros)
002600011011
002700011011      * Impostazione iniziale parametri
002800011011     C                   Exsr      InzVar
002900011011
003000011011      * Controllo parametri
003100011011     C                   Exsr      CtrlParms
003200011011
003300011011      * Controllo decimali
003400011012      * (se consentiti al massimo -9- è inutile controllare !!!)
003500011012     C                   if        IcdNrd < 9
003600011011     C                   exsr      CtrDec
003700011011     C                   endif
003800011011
003900011011      * Fine programma
004000011011     C                   Exsr      Endpgm
004100011011
004200011011      *----------------------------------------------------
004300011011      * Inizializzazione variabili
004400011011      *----------------------------------------------------
004500011011     C     InzVar        Begsr
004600011011      * Impostazione iniziale parametri di output
004700011011     C                   eval      OcdEsi    = *off
004800011011     C                   Endsr
004900011011      *----------------------------------------------------
005000011011      * Controllo parametri passati
005100011011      *----------------------------------------------------
005200011011     C     CtrlParms     Begsr
005300011011     C                   Select
005400011012     C                   When      IcdNrd < 0 or IcdNrd > 9
005500011011     C                   eval      OcdEsi = '9'
005600011011     C                   EndSl
005700011011     C*
005800011011     C     OcdEsi        casNE     *off          EndPgm
005900011011     C                   EndCs
006000011011     C                   Endsr
006100011011      *----------------------------------------------------
006200011011      * Controllo Decimali
006300011011      *----------------------------------------------------
006400011011     C     CtrDec        Begsr
006500011012     C                   z-add     IcdNum        wNumN
006600011012     C                   movel     wNumN         wNumA
006700011011     C*
006800011011     C                   if        %subst(wNumA:IcdNrd+1) = *all'0'
006900011011     C                   eval      OcdEsi = '0'
007000011011     C                   else
007100011011     C                   eval      OcdEsi = '1'
007200011011     C                   endif
007300011011     C                   ENDSR
007400011011      *----------------------------------------------------
007500011011      * Fine programma
007600011011      *----------------------------------------------------
007700011011     C     Endpgm        Begsr
007800011011     C     OcdEsi        Casne     *off          MsgErr
007900011011     C                   Endcs
008000011012     C                   movel     *on           *inRT
008100011011     C                   Return
008200011011     C                   Endsr
008300011011      *----------------------------------------------------
008400011011      * Impostazione eventuale messaggio di errore in uscita
008500011011      *----------------------------------------------------
008600011011     C     MsgErr        Begsr
008700011011   B1C                   Select
008800011011   _1C                   When      OcdEsi = '1'
008900011011   B2C                   If        IcdNrd = 0
009000011011     C                   eval      OcdMsg = MSG(1)
009100011011   X2C                   else
009200011011     C                   clear                   wNrDec
009300011011   B3C                   if        IcdNrd < 10
009400011011     C                   move      IcdNrd        wNrDec1
009500011011     C                   movel     wNrDec1       wNrDec
009600011011   X3C                   else
009700011011     C                   move      IcdNrd        wNrDec2
009800011011     C                   movel     wNrDec2       wNrDec
009900011011   E3C                   Endif
010000011011     C                   eval      OcdMsg = %trim(MSG(2)) + ' ' +
010100011011     C                                      %trim(wNrDec) + ' ' +
010200011011     C                                      %trim(MSG(3))
010300011011   E2C                   Endif
010400011011   _1C                   When      OcdEsi = '9'
010500011011     C                   movel     MSG(4)        OcdMsg
010600011011   E1C                   Endsl
010700011011     C                   Endsr
010800011011      *----------------------------------------------------
010900011011      * Routine iniziale
011000011011      *----------------------------------------------------
011100011011     C     *INZSR        Begsr
011200011011     C     *ENTRY        PLIST
011300011012     C                   Parm                    XDECDS
011400011011     C                   Endsr
011500011011** MSG
011600011012NON SONO AMMESSI DECIMALI PER LA DIVISA INDICATA
011700011012SONO AMMESSI AL MASSIMO
011800011012 DECIMALI PER LA DIVISA INDICATA
011900011012NUMERO DI CIFRE DECIMALI CONSENTITE ERRATO
