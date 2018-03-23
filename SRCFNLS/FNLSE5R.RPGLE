000100020909     H DECEDIT('0,') DATEDIT(*YMD.)
000200010404      *---------------------------------------------------------------*
000300020905      * Pulizia BOLLE da CLIENTE -TESTATA-(archivi vecchi)            *
000400010404      *---------------------------------------------------------------*
000500020909     FAzOrg01l  if   e           k disk
000600020905     FFnVab00f  uf   e             disk    usropn
000700020905     FFnVad00f  uf   e             disk    usropn
000800020905     FFnVat00f  uf   e             disk    usropn
000900020905     FFiVab00f  o    e             disk
001000020905     FFiVad00f  o    e             disk
001100020905     FFiVat00f  o    e             disk
001200020909      *-------------------
001300020909      * DS ESTERNE
001400020909      *-------------------
001500020909     D FnlsE5ds      e ds
001600020909     D Og143         e ds
001700020909     D Fnlv55ds      e ds
001800020909      *-------------------
001900020909      * DS PARAMETRI UTENTE
002000020909      *-------------------
002100020905     D tibs34ds      e ds                  inz
002200020905     D ddatiute      e ds
002300020905     D azuteds       e ds                  extname(AzUte00F)
002400010404      *---------------------------------------------------------------*
002500010404      *                    M  A  I  N                                 *
002600010404      *---------------------------------------------------------------*
002700020905      *
002800020905      * Procedo se richiesta elaborazione, se non ho errori
002900020909    1C                   IF        IE5TLA <> 'C' and OE5ERR <>'E'
003000020905      *
003100020905      * VAB
003200020905     C                   move      simfel        vabfgs
003300020905     C                   read      FnVab000                               30
003400020905     C     *in30         DOWEQ     *off
003500020905      *
003600020905      * Scrivo record su nuovo file bolle
003700020905     C                   write     FiVab000
003800020905      *
003900020905      * Cancello record dal vecchio file bolle
004000020905     C                   delete    FnVab000
004100020905      *
004200020905     C                   read      FnVab000                               30
004300020905     C                   ENDDO
004400020905      *
004500020905      * VAD
004600020905     C                   move      simfel        vadfgs
004700020905     C                   read      FnVad000                               30
004800020905     C     *in30         DOWEQ     *off
004900020905      *
005000020905      * Scrivo record su nuovo file bolle
005100020905     C                   write     FiVad000
005200020905      *
005300020905      * Cancello record dal vecchio file bolle
005400020905     C                   delete    FnVad000
005500020905      *
005600020905     C                   read      FnVad000                               30
005700020905     C                   ENDDO
005800020905      *
005900020905      * VAT
006000020905     C                   move      simfel        vatfgs
006100020905     C                   read      FnVat000                               30
006200020905     C     *in30         DOWEQ     *off
006300020905      *
006400020905      * Scrivo record su nuovo file bolle
006500020905     C                   write     FiVat000
006600020905      *
006700020905      * Cancello record dal vecchio file bolle
006800020905     C                   delete    FnVat000
006900020905      *
007000020905     C                   read      FnVat000                               30
007100020905     C                   ENDDO
007200020905     C                   ENDIF
007300020905      *
007400020905      * Fine programma
007500020909    1C                   IF        IE5TLA <> *blanks or OE5ERR = 'E'
007600020905     C                   seton                                        LR
007700020905     C                   else
007800020905     C                   seton                                        RT
007900020905     C                   ENDIF
008000010404      *---------------------------------------------------------------*
008100010404      * ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
008200010404      *---------------------------------------------------------------*
008300010404     C     *inzsr        BEGSR
008400010404      *
008500930804     C     *ENTRY        PLIST
008600020905     C                   PARM                    Fnlse5ds
008700010404      *
008800020905      * Open files
008900020905     C                   open      fnvab00f                             20
009000020909     C                   open      fnvad00f                             21
009100020909     C                   open      fnvat00f                             22
009200020905      *
009300020905      * Segnalo l'errore
009400020909   1aC                   IF        *in20 =*on  or *in21=*on or *in22=*on
009500020909     C                   move      'E'           OE5ERR
009600020909      *
009700020909   1eC                   ELSE
009800020909      * Recupero dati utente
009900020905     C                   EXSR      DatiJob
010000020909      *
010100020909   2aC                   IF        IE5POU = 0
010200020909     C                   move      DutPou        IE5POU
010300020909   2-C                   ENDIF
010400020909      *
010500020909      * Controllo il P.O di gestione: non deve essere nè sede nè poste
010600020909     C                   EXSR      CtrPoU
010700020909   1-C                   ENDIF
010800020905      *
010900930804     C                   ENDSR
011000020905      *----------------------------------------------------------------
011100020905      *  Reperisco Dati del job (Utente/Operativi)
011200020905      *----------------------------------------------------------------
011300020905     C     DatiJob       BegSr
011400020905      *
011500020905     C     *dtaara       define    §azute        azuteds
011600020905     C     *dtaara       define    §datiute      ddatiute
011700020905      *
011800020905     C                   in(E)     *dtaara
011900020905     C                   IF        %Error
012000020905     C                   call      'TIBS34R'
012100020905     C                   parm                    Tibs34Ds
012200020905     C                   in        *dtaara
012300020905     C                   ENDIF
012400020905      *
012500020905     C                   endsr
012600020909      *----------------------------------------------------------------
012700020909      *  Controllo P.O di gestione non sede e non poste
012800020909      *----------------------------------------------------------------
012900020909     C     CtrPou        BegSr
013000020909      *
013100020909      *  Se il P.O gestione è uguale al P.O utente controllo i dati
013200020909      *  della *dataara
013300020909   1aC                   IF        DutPou = IE5POU
013400020909      *
013500020909   2aC                   if        DutNtw ='PPT' or DutLpo ='S'
013600020909     C                             or O34Err <> ' '  or DutErr <> ' '
013700020909     C                   move      'E'           OE5ERR
013800020909   2-C                   endif
013900020909      *
014000020909   1eC                   ELSE
014100020909      * Se <> controllo P.O di gestione passato in input.Imposto Simfel
014200020909   2aC                   if        IE5POU = 46 or IE5POU = *zeros
014300020909     C                   move      'E'           OE5ERR
014400020909   2eC                   else
014500020909      *
014600020909      *  aggancio organigramma
014700020909     C                   clear                   Og143
014800020909     C     IE5POU        chain     AzOrg01l                           31
014900020909   3aC                   if        *in31 = *off and OrgFva = *blanks
015000020909     C                   eval      Og143 = OrgDe3
015100020909   3-C                   endif
015200020909     C
015300020909   3aC                   if        §OgNtw = 'PPT' or §ogNtw= *blanks
015400020909     C                   move      'E'           OE5ERR
015500020909   3eC                   else
015600020909      * imposto SimFel
015700020909     c                   clear                   fnlv55ds
015800020909     c                   eval      d55tla = 'L'
015900020909     c                   eval      d55tpt = 'P'
016000020909     c                   eval      d55lin = IE5POU
016100020909     c                   eval      d55drf = *date
016200020909     c                   call      'FNLV55R'
016300020909     c                   parm                    fnlv55ds
016400020909   4ac                   if        d55err = *blanks
016500020909     C                   move      d55tfp        SimFel
016600020909   4eC                   else
016700020909     C                   move      'E'           OE5ERR
016800020909   4-c                   endif
016900020909   3-C                   endif
017000020909   2-C                   endif
017100020909     C
017200020909   1-C                   ENDIF
017300020909      *
017400020909     C                   ENDSR
