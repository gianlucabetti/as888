000100000000/*-------------------------------------------------------------------*/
000200000000/*                                                                   */
000300000000/*  Compile options:                                                 */
000400000000/*                                                                   */
000500081021/*    CrtCmd Cmd(UBIFSLCK) PGM(UBIFSLCKC)                            */
000600081021/*           SrcMbr(UBIFSLCKX) HLPPNLGRP(UBIFSLCKH)                  */
000700081021/*           HlpId(*CMD)                                             */
000800000000/*                                                                   */
000900000000/*-------------------------------------------------------------------*/
001000081021     Cmd        Prompt( 'Display IFS Object Locks' )
001100000000
001200000000     Parm       IFSOBJ   *Pname     300               +
001300000000                Min( 1 )                              +
001400000000                Expr( *YES )                          +
001500000000                Vary( *YES *INT2 )                    +
001600000000                Case( *MIXED )                        +
001700000000                Prompt( 'IFS object' )
001800000000
001900000000     Parm       OUTPUT   *Char        3               +
002000000000                Rstd( *YES )                          +
002100000000                Dft( * )                              +
002200081021                SpcVal(( * DSP ) ( *PRINT  PRT )      +
002300081021                       ( *BATCH BCH))                 +
002400000000                Prompt( 'Output' )
002500081021
002600081021     Parm       Kwd(LCKSTS)                           +
002700081021                Type(*CHAR)                           +
002800081021                Len(4)                                +
002900081021                Rtnval(*YES)                          +
003000081021                Prompt('Var. CL per LCKSTS  (4)')
