# `explicitSource` function that nh2 is upstreaming
# in https://github.com/NixOS/nixpkgs/pull/56985
# and dogfooding here.
{ lib }: rec {
  inherit (lib) cleanSourceWith;

  # Splits a filesystem path into its components.
  splitPath = path: lib.splitString "/" (toString path);

  # Turns a list of path components into a tree, e.g.
  #
  #     ["a" "b" "c1"]
  #     ["a" "b" "c2"]
  #     ["a" "b" "c3"]
  #     ["a" "x"     ]
  #
  # becomes:
  #
  #     { a = { b = { c1 = null; c2 = null; c3 = null }; x = null; }; }
  pathComponentsToTree = paths: with lib;
    foldl (tree: path: recursiveUpdate tree (setAttrByPath path null)) {} paths;

  # Returns true if and only if any prefix of the given `path` leads to a leaf
  # (`null`) in the given `tree` (nested attrset).
  # That is: "If we go down the tree by the given path, do we hit a leaf?"
  #
  # Example: For the tree `tree`
  #
  #     a
  #       b-leaf
  #       c-leaf
  #       d
  #         e-leaf
  #     f-leaf
  #
  # represented as attrset
  #
  #     { a = { b = null; c = null; d = { e = null; }; }; f = null; }
  #
  # we have (string quotes omitted for readability):
  #
  #     isPrefixOfLeafPath [a]       tree == false
  #     isPrefixOfLeafPath [x]       tree == false
  #     isPrefixOfLeafPath [a b]     tree == true
  #     isPrefixOfLeafPath [a b c]   tree == true
  #     isPrefixOfLeafPath [a b c x] tree == true
  #     isPrefixOfLeafPath [a d]     tree == false
  isPrefixOfLeafPath = path: tree:
    if tree == null
      then true
      else
        if path == []
          then false
          else
            let
              component = builtins.head path;
              restPath = builtins.tail path;
            in
              if !(builtins.hasAttr component tree)
                then false
                else
                  let
                    subtree = builtins.getAttr component tree;
                  in
                    isPrefixOfLeafPath restPath subtree;



  # See `explicitSource` for an example of this this filter.
  #
  # You can use this filter standalone (with `builtins.filterSource`
  # or better, `builtins.path` with explicitly given `name`) when
  # you want to combine it with other filters.
  explicitSourceFilter =
    {
      # List of dirs under which all recursively contained files are taken in
      # (unless a file is filtered by other arguments).
      # Dirs that match explicitly are immediately taken in.
      includeDirs ? [],
      # Explicit list of files that should be taken in.
      includeFiles ? [],
      # Exclude dotfiles/dirs by default (unless they are matched explicitly)?
      excludeHidden ? true,
      # If any of the path components given here appears anywhere in the path,
      # (e.g. X in `.../X/...`), the path is excluded (unless matched explicitly).
      # Example: `pathComponentExcludes = [ "gen" "build" ]`.
      pathComponentExcludes ? [],

      # Debugging

      # Enable this to enable a `builtins.trace` output that prints which files
      # were matched as source inputs.
      # Output looks like:
      #     trace: myproject: include regular   /home/user/myproject/Setup.hs
      #     trace: myproject: skip    regular   /home/user/myproject/myproject.nix
      #     trace: myproject: skip    directory /home/user/myproject/dist
      #     trace: myproject: include directory /home/user/myproject/images
      #     trace: myproject: include regular   /home/user/myproject/images/image.svg
      #     trace: myproject: include directory /home/user/myproject/src
      #     trace: myproject: include directory /home/user/myproject/src/MyDir
      #     trace: myproject: include regular   /home/user/myproject/src/MyDir/File1.hs
      #     trace: myproject: include regular   /home/user/myproject/src/MyDir/File2.hs
      debugTraceEnable ? false,
      # Set this to prefix the trace output with some arbitrary string.
      # Useful if you enable `debugTraceEnable` in multiple places and want
      # to distinguish them.
      debugTracePrefix ? "",

      # For debugging
      name,
    }: with lib;
      let
        # Pre-processing done once, across all files passed in.

        # Turns a list into a "set" (map where all values are null).
        keySet = list: genAttrs list (name: null);

        # For fast non-O(n) lookup, we turn `includeDirs` and `includeFiles` into
        # string-keyed attrsets first.
        includeDirsSet = keySet (map toString includeDirs);
        srcFilesSet = keySet (map toString includeFiles);

        # We also turn `includeDirs` into a directory-prefix-tree so that we can
        # check whether a given path is under one of the `includeDirs` in sub-O(n).
        includeDirsTree = pathComponentsToTree (map splitPath includeDirs);
      in
        # The actual filter function with per-file processing
        fullPath: type:
          let
            fileName = baseNameOf (toString fullPath);

            components = splitPath fullPath;

            isExplicitSrcFile = hasAttr fullPath srcFilesSet;
            isExplicitSrcDir = type == "directory" && hasAttr fullPath includeDirsSet;
            # The below is equivalent to
            #   any (srcDir: hasPrefix (toString srcDir + "/") fullPath) includeDirs;
            # but faster than O(n) where n is the number of `includeDirs` entries.
            isUnderSomeSrcDir = isPrefixOfLeafPath components includeDirsTree;

            isHidden = excludeHidden && hasPrefix "." fileName;

            hasExcludedComponentInPath = any (c: elem c pathComponentExcludes) components;

            isSourceInput =
              isExplicitSrcFile ||
              isExplicitSrcDir ||
              (isUnderSomeSrcDir && !isHidden && !hasExcludedComponentInPath);

            tracing =
              let
                prefix = if debugTracePrefix == "" then "" else debugTracePrefix + ": ";
                action = if isSourceInput then "include" else "skip   ";
                # Pad type (e.g. "regular", "symlink") to be as wide as
                # the widest ("directory") for aligned output.
                width = max (stringLength "directory") (stringLength type);
                formattedType = substring 0 width (type + "         ");
              in
              debug.traceIf
                debugTraceEnable
                "${prefix}${action} ${formattedType} ${fullPath}";
          in
            tracing isSourceInput;

  # A general-purpose, explicit source code importer suitable for most
  # packaging needs.
  #
  # See `explicitSourceFilter` for an explanation of the filter arguments.
  #
  # Example usage:
  #
  #    src = lib.explicitSource ./. {
  #      name = "mypackage";
  #      includeDirs = [
  #        ./src
  #        ./app
  #        ./images
  #      ];
  #      includeFiles = [
  #        ./mypackage.cabal
  #        ./Setup.hs
  #      ];
  #      pathComponentExcludes = [ "build" "gen" ];
  #    };
  #
  # Note that `includeDirs = [ ./. ]` is also permitted.
  #
  # But consider that in most cases you will not want to include files
  # that are not relevant for the build, such as `.nix` files, so that
  # input hashes do not change unnecessarily.
  #
  # If you want to combine it with other filters, use `explicitSourceFilter`
  # directly instead.
  explicitSource = topPath: filterArgs@{
    # Recommended, to be identifiable in downloads and `nix-store -qR`.
    # "-src" will be automatically appended.
    name ? "filtered",
    # Other `explicitSourceFilter` arguments.
    ...
  }:
    cleanSourceWith {
      # The `-src` suffix makes it easy to distinguish source store paths
      # from built output store paths in the nix store.
      #
      # Requiring an explicit name prevents the basename of the directory
      # making it into the store path when `./.` is used, thus preventing
      # impure builds in situations where ./. is a directory with random
      # names, as is common e.g. when cloning source repositories under
      # multiple names; see https://github.com/NixOS/nix/issues/1305
      # and https://github.com/NixOS/nixpkgs/pull/67996.
      name = name + "-src";
      src = topPath;
      filter = explicitSourceFilter filterArgs;
    };
}
