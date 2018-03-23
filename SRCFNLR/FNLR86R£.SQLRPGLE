000100070514      *PARMS CLOSQLCSR(*ENDMOD) DYNUSRPRF(*OWNER) DBGVIEW(*SOURCE)
000200070514      *===============================================================*
000300070514      *?FNLR86R - Pulizia  LOG di TELLUS                             ?*
000400070514      *===============================================================*
000500070514      *
000600070514     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700070514     h alwnull(*inputonly)
000800070514      *
000900070514      *---------------------------------------------------------------*
001000070514      *
001100070514      * Parametri
001200070514     d KPJBA         e ds
001300070514      *
001400070514      * Parametri ricevuti dal pgm chiamante (FNLR83R)
001500070514     d Param           ds
001600070514      * - Data pulizia log Tellus
001700070514     d  ComTlLg                1      8  0
001800070514      * - Data pulizia bolle arrivi (con LNA NON estera)
001900070514     d**ComDPB                17     24  0
002000070514      * - Data pulizia bolle di transito
002100070514     d**ComDPT                25     32  0
002200070514      * - Data pulizia f.viaggio partenza
002300070514     d**ComFGV                35     42  0
002400070514      * - Data pulizia fogli vari
002500070514     d**ComFVV                43     50  0
002600070514      * - Data pulizia f.viaggio partenza in arrivo
002700070514     d**ComFVA                51     58  0
002800070514     *** * - Data pulizia statistiche arrivi
002900070514     ***d**ComDPS                60     67  0
003000070514      * - Data pulizia bolle arrivi (con LNA estera - bolle export)
003100070514     d**ComPBE                68     75  0
003200070514      * - Data pulizia bolle arrivi con riserva
003300070514     d**ComARR               191    198  0
003400070514      * - Data pulizia bolle arrivi con C.A.
003500070514     d**ComBCA               199    206  0
003600070514      * - Data pulizia bolle arrivi                - POSTE
003700070514     d**PosDPB               207    214  0
003800070514      * - Data pulizia bolle arrivi - colle export - POSTE
003900070514     d**PosPBE               215    222  0
004000070514      * - Data pulizia bolle arrivi con riserva    - POSTE
004100070514     d**PosARR               223    230  0
004200070514      * - Data pulizia bolle arrivi con C.A.       - POSTE
004300070514     d**PosBCA               231    238  0
004400070514      * - Data pulizia bolle con reclami
004500070514     d**ComREC               239    246  0
004600070514      *
004700070514      *---------------------------------------------------------------*
004800070514      *
004900070514     c     *Entry        plist
005000070514     c                   parm                    KPJBA
005100070514     c                   movel     KPJBU         Param
005200070514      *
005300070514      * Impostazione opzioni SQL
005400070514      *
005500070514     c/exec sql
005600070514     c+     set  option  DynUsrPrf = *Owner,
005700070514     c+                  CloSqlCsr = *EndMod
005800070514     c/end-exec
005900070514      *
006000070514      * Esecuzione SQL
006100070514      *
006200070514     c/exec sql
006300070514     c+     DELETE from FILTE00F
006400070514     c+     where  LTEdat <= :ComTlLg
006500070514     c/end-exec
006600070514      *
006700070514     c                   eval      *inLR = *on
