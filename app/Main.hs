{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified Graphics.UI.Gtk as Gtk

import Control.Monad

import Lib
import Data.IORef
import qualified Control.Lens as Lens
import qualified Data.IntMap as IntMap
import Data.Typeable

import Data.Maybe (fromJust)

data Element = Button {
    text :: String,
    onClick :: State --and here the fun begins
} | Text {
    text :: String
} | HBox {
    children :: [Element]
} | forall props state . (Show state, Typeable state) => CustomComponent (ActualCustomComponent props state)


data CustomButtonProps = CustomButtonProps {
    count :: Int,
    incrementCount :: State -> State
}

data CustomButtonState = CustomButtonState {
-- Stateless
} deriving (Show)

buttonRender :: CustomButtonProps -> CustomButtonState -> Lens.Lens' State CustomButtonState -> Element
buttonRender props state updateLens = Button {
    text = "Count: " ++ show (count props),
    onClick =  (incrementCount props)
}


buttonComponent = StatefulComponent {
    initialState = CustomButtonState { },
    render = buttonRender
}


data MainProps = MainProps {
}

data MainState = MainState {
    counts :: [Int]
} deriving Show

mainRender :: MainProps -> MainState -> Lens.Lens' State MainState -> Element
mainRender props state updateLens = HBox {
        children = map createButton [0..4]
    }
    where
        createButton index = CustomComponent (ActualCustomComponent {
            the_type = buttonComponent,
            the_props = CustomButtonProps {
                count = ((counts state) !! index),
                incrementCount = Lens.set updateLens MainState {counts = incrementCount index}
            }
        })

        incrementCount index = x ++ [current +1] ++ ys
            where
                (x,current:ys) = splitAt index (counts state)


mainComponent = StatefulComponent {
    initialState = MainState { counts = replicate 5 0 },
    render = mainRender
}

rootElement :: Element
rootElement = CustomComponent (ActualCustomComponent {
    the_type = mainComponent,
    the_props = MainProps {}
})


unsafeFromJust :: Lens.Lens' (Maybe a) a
unsafeFromJust = Lens.lens fromJust setJust
 where
  setJust (Just _) b = Just b
  setJust Nothing  _ = error "setJust: Nothing"

data ActualCustomComponent props state = ActualCustomComponent {
    the_type :: StatefulComponent props state,
    the_props :: props
}

data StatefulComponent props state = StatefulComponent {
    initialState :: state,
    render :: props -> state -> Lens.Lens' State state -> Element
}

data ActualCustomComponentState state = (Show state) => ActualCustomComponentState {
    _myState :: state,
    _childState :: State
}

instance Show (ActualCustomComponentState a) where
    show (ActualCustomComponentState{_myState=_myState, _childState=_childState}) = "Custom state: " ++ show _myState ++ " " ++ show _childState

data State = HBoxState {
    _hbox :: Gtk.HBox,
    _childrenState :: [State]
} | ButtonState {
    _button :: Gtk.Button,
    _handler :: Gtk.ConnectId Gtk.Button
} | TextState {
    _label :: Gtk.Label
} | forall state . (Show state, Typeable state) => CustomComponentState (ActualCustomComponentState state)

Lens.makeLenses ''State

getWidget :: State -> Gtk.Widget
getWidget HBoxState {_hbox=hbox} = Gtk.toWidget hbox
getWidget TextState {_label=label} = Gtk.toWidget label
getWidget (CustomComponentState a) = helper a where
    helper :: ActualCustomComponentState state -> Gtk.Widget
    helper (ActualCustomComponentState{_childState=childState}) = getWidget childState
getWidget ButtonState {_button=button} = Gtk.toWidget button

instance Show State where
    show HBoxState{_hbox=hbox, _childrenState=childrenState} = "HBoxState {" ++ show(childrenState) ++ "}"
    show (CustomComponentState a) = show a
    show TextState {} = "Text {}"
    show ButtonState {} = "Button {}"

childState :: Lens.Lens' State State
childState = Lens.lens getState setState
    where
        setState (CustomComponentState a) b = (CustomComponentState a {_childState = b})

        getState (CustomComponentState a) = _childState a

myState :: (Show a, Typeable a) => Lens.Lens' State a
myState = Lens.lens getState setState
    where
        setState (CustomComponentState a) b = (CustomComponentState ActualCustomComponentState {
            _myState = b,
            _childState = (_childState a)
        })

        getState (CustomComponentState a) = fromJust $ cast (_myState a)

processStateUpdate :: IORef (Maybe State) -> (State -> State) -> IO ()
processStateUpdate root updateFunction = do
    current <- readIORef root
    let actual = fromJust current
    let next = updateFunction actual
    afterUpdate <- updateState (processStateUpdate root) id rootElement next
    writeIORef root $ Just afterUpdate
    print afterUpdate

updateHelper :: (Show state, Typeable state) => ((State -> State) -> IO ()) -> Lens.Lens' State State -> ActualCustomComponent props state -> ActualCustomComponentState state-> IO State
updateHelper callback stateLens actualComponent actualComponentState = do
    let startingState = (_myState actualComponentState)
    let resultingGui = (render (the_type actualComponent)) (the_props  actualComponent) startingState (stateLens . myState)

    let totalLense = Lens.unsafeSingular (stateLens . childState) :: Lens.Lens' State State

    childState <- updateState callback totalLense resultingGui (_childState actualComponentState)

    let newState = CustomComponentState ActualCustomComponentState {
        _myState = startingState,
        _childState = childState
    }

    return newState


updateState :: ((State -> State) -> IO ()) -> Lens.Lens' State State -> Element -> State -> IO State
updateState callback stateLens (HBox {children=children}) (a@HBoxState{}) = do
    let theHbox = _hbox a

    chilrenStates <- zipWithM (createStateHelper theHbox) [0..] children

    let newState = HBoxState {
        _hbox = theHbox,
        _childrenState = chilrenStates
    }

    let childWidgets = map getWidget chilrenStates
    oldChildWidgets <- Gtk.containerGetChildren theHbox

    when (childWidgets /= oldChildWidgets) $ do
        mapM_ (Gtk.containerRemove theHbox) oldChildWidgets
        mapM_ (Gtk.containerAdd theHbox) childWidgets
        Gtk.widgetShowAll theHbox

    return newState

    where
        totalLense :: Lens.Lens' State [State]
        totalLense = Lens.unsafeSingular (stateLens . childrenState)

        createStateHelper :: (Gtk.ContainerClass c) => c -> Int -> Element -> IO State
        createStateHelper theHbox index element = do
            let childState = (_childrenState a) !! index
            state <- updateState callback (Lens.unsafeSingular  (totalLense . (Lens.ix index))) element childState
            return state

updateState callback stateLens (CustomComponent a) (CustomComponentState b) =
    case (cast b) of
        Just result -> updateHelper callback stateLens a result
        Nothing -> createState callback stateLens (CustomComponent a)

updateState callback stateLens (Text {text=text}) (TextState{_label=theTextComponent}) = do
    Gtk.set theTextComponent [(Gtk.:=) Gtk.labelLabel text]
    return TextState {_label = theTextComponent}

updateState callback stateLens (Button {text=text,onClick=onClick}) (ButtonState{_button=theButton,_handler=_handler}) = do
    Gtk.set theButton [(Gtk.:=) Gtk.buttonLabel text]
    Gtk.signalDisconnect _handler
    handler <- Gtk.on theButton Gtk.buttonActivated $ callback onClick
    return ButtonState {_button = theButton, _handler = handler}

-- Deal with creating it if it doesn't match any of the above cases
updateState callback stateLens a _ = do
    createState callback stateLens a

createState :: ((State -> State) -> IO ()) -> Lens.Lens' State State -> Element -> IO State
createState callback stateLens (HBox {children=children}) = do
    theHbox <- Gtk.hBoxNew True 10

    chilrenStates <- zipWithM (createStateHelper theHbox) [0..] children

    let newState = HBoxState {
        _hbox = theHbox,
        _childrenState = chilrenStates
    }

    return newState

    where
        totalLense :: Lens.Lens' State [State]
        totalLense = Lens.unsafeSingular (stateLens . childrenState)

        createStateHelper :: (Gtk.ContainerClass c) => c -> Int -> Element -> IO State
        createStateHelper theHbox index element = do
            state <- createState callback (Lens.unsafeSingular  (totalLense . (Lens.ix index))) element
            hlep (getWidget state)
            return state
            where
                hlep :: Gtk.Widget -> IO ()
                hlep = Gtk.containerAdd theHbox

createState callback stateLens (CustomComponent ActualCustomComponent{the_type=the_type, the_props=the_props}) = do
    let startingState = (initialState the_type)
    let resultingGui = (render the_type) the_props startingState (stateLens . myState)

    let totalLense = Lens.unsafeSingular (stateLens . childState) :: Lens.Lens' State State

    childState <- createState callback totalLense resultingGui

    let newState = CustomComponentState ActualCustomComponentState {
        _myState = startingState,
        _childState = childState
    }

    return newState

createState callback stateLens (Text {text=text}) = do
    theTextComponent <- Gtk.labelNew (Just text)
    return TextState {_label = theTextComponent}

createState callback stateLens (Button {text=text,onClick=onClick}) = do
    theButton <- Gtk.buttonNewWithLabel text

    handler <- Gtk.on theButton Gtk.buttonActivated $ callback onClick

    return ButtonState {_button = theButton, _handler = handler}

main :: IO ()
main = do
    Gtk.initGUI

    theWindow <- Gtk.windowNew
    Gtk.set theWindow [(Gtk.:=) Gtk.windowTitle "Main title!"]

    rootState <- newIORef Nothing

    let callback = processStateUpdate rootState

    startingState <- createState callback id rootElement
    writeIORef rootState (Just startingState)

    print startingState

    Gtk.containerAdd theWindow (getWidget startingState)

    Gtk.widgetShowAll theWindow

    Gtk.mainGUI