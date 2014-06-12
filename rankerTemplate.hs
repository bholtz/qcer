    putStrLn "Running auto-generated testing for %%fnName%%:%%propName%%"
    let prop%%propName%% %%vb%% = %%prop%%
          where types = %%vb%% :: %%vbtype%%
    quickCheckResult prop%%propName%%
